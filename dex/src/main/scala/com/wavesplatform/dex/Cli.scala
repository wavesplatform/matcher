package com.wavesplatform.dex

import java.io.File
import java.nio.charset.StandardCharsets
import java.util.{Base64, Scanner}

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.db.AccountStorage
import scopt.{OParser, RenderingMode}

import scala.util.{Failure, Success, Try}

object Cli {
  // todo commands:
  // base64
  // get account by seed [and nonce]
  def main(rawArgs: Array[String]): Unit = {
    val builder = OParser.builder[Args]
    val parser = {
      import builder._
      OParser.sequence(
        programName("dex-cli"),
        head("DEX CLI", Version.VersionString),
        cmd(Command.CreateAccountStorage.name)
          .action((_, s) => s.copy(command = Some(Command.CreateAccountStorage)))
          .text("Creates an encrypted account storage")
          .children(
            opt[File]("create-account-storage-directory")
              .abbr("cas-dir")
              .text("The directory for account.dat file")
              .required()
              .action((x, s) => s.copy(createAccountStorageDir = x)),
            opt[SeedFormat]("create-account-storage-seed-format")
              .abbr("cas-seed-format")
              .text("The format of seed to enter, 'raw-string' by default")
              .valueName("<raw-string,base64>")
              .action((x, s) => s.copy(createAccountStorageSeedFormat = x)),
            opt[Int]("create-account-storage-account-nonce")
              .abbr("cas-account-nonce")
              .text("The nonce for account, the default value means you entered the account seed")
              .valueName("<number>")
              .action((x, s) => s.copy(createAccountStorageAccountNonce = Some(x)))
          )
      )
    }

    OParser.parse(parser, rawArgs, Args()).foreach { args =>
      args.command match {
        case None => println(OParser.usage(parser, RenderingMode.TwoColumns))
        case Some(command) =>
          println(s"Running '${command.name}' command")
          command match {
            case Command.CreateAccountStorage =>
              val accountFile = args.createAccountStorageDir.toPath.resolve("account.dat").toFile.getAbsoluteFile
              if (accountFile.isFile) {
                System.err.println(s"The '$accountFile' is already exist. If you want to create a file with a new seed, delete the file before.")
                System.exit(1)
              }

              val seedPromptText = s"Enter the${if (args.createAccountStorageAccountNonce.isEmpty) " seed of DEX's account" else " base seed"}: "
              val rawSeed        = readSeedFromFromStdIn(seedPromptText, args.createAccountStorageSeedFormat)
              val password       = readSecretFromStdIn("Enter the password for file: ")
              val accountSeed    = args.createAccountStorageAccountNonce.fold(rawSeed)(AccountStorage.getAccountSeed(rawSeed, _))
              AccountStorage.save(
                accountSeed,
                AccountStorage.Settings.EncryptedFile(
                  accountFile,
                  password
                )
              )

              println(s"""Saved the seed to '$accountFile'.
                         |Don't forget to update your settings:
                         |
                         |waves.dex {
                         |  account-storage {
                         |    type = "encrypted-file"
                         |    encrypted-file {
                         |      path = "$accountFile"
                         |      password = "paste-entered-password-here"
                         |    }
                         |  }
                         |}
                         |""".stripMargin)
          }
          println("Done")
      }
    }
  }

  private sealed trait Command {
    def name: String
  }

  private object Command {
    case object CreateAccountStorage extends Command {
      override def name: String = "create-account-storage"
    }
  }

  private sealed trait SeedFormat
  private object SeedFormat {
    case object RawString extends SeedFormat
    case object Base64    extends SeedFormat

    implicit val weekDaysRead: scopt.Read[SeedFormat] = scopt.Read.reads {
      case "raw-string" => RawString
      case "base64"     => Base64
      case x            => throw new IllegalArgumentException(s"Expected 'raw-string' or 'base64', but got '$x'")
    }
  }

  private val defaultFile = new File(".")
  private case class Args(command: Option[Command] = None,
                          createAccountStorageDir: File = defaultFile,
                          createAccountStorageSeedFormat: SeedFormat = SeedFormat.RawString,
                          createAccountStorageAccountNonce: Option[Int] = None)

  private def readSeedFromFromStdIn(prompt: String, format: SeedFormat): ByteStr = {
    val rawSeed = readSecretFromStdIn(prompt)
    format match {
      case SeedFormat.RawString => rawSeed.getBytes(StandardCharsets.UTF_8)
      case SeedFormat.Base64 =>
        Try(Base64.getDecoder.decode(rawSeed)) match {
          case Success(r) => r
          case Failure(_) =>
            System.err.println("Can't parse the seed in the base64 format, try again")
            readSeedFromFromStdIn(prompt, format)
        }
    }
  }

  private def readSecretFromStdIn(prompt: String): String = {
    val r = Option(System.console()) match {
      case Some(console) => new String(console.readPassword(prompt))
      case None =>
        System.out.print(prompt)
        val scanner = new Scanner(System.in)
        scanner.nextLine()
    }
    if (r.isEmpty) {
      System.err.println("Please enter a non-empty password")
      readSecretFromStdIn(prompt)
    } else r
  }
}
