import java.io.{BufferedInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.io.IOUtils
import sbt.Keys._
import sbt._

// See https://github.com/swagger-api/swagger-ui/issues/5710
object RewriteSwaggerConfigPlugin extends AutoPlugin {
  override val trigger = PluginTrigger.NoTrigger
  override def projectSettings: Seq[Def.Setting[_]] =
    inConfig(Compile)(
      Seq(
        resourceGenerators += Def.task {
          val file = resourceManaged.value / "META-INF" / "resources" / "webjars" / "swagger-ui" / "3.24.3" / "index.html"
          val log  = streams.value.log

          val html = (Compile / dependencyClasspath).value
            .find(_.data.getName == "swagger-ui-3.24.3.jar")
            .flatMap(jar => fileContentFromJar(jar.data, "META-INF/resources/webjars/swagger-ui/3.24.3/index.html"))
            .map { new String(_, StandardCharsets.UTF_8) }

          val resource = "swagger-ui-3.24.3.jar:META-INF/resources/webjars/swagger-ui/3.24.3/index.html"
          html match {
            case None => throw new RuntimeException(s"Can't find $resource")
            case Some(html) =>
              log.info(s"Found $resource")
              val doc = org.jsoup.parser.Parser.parse(html, "127.0.0.1")
              import scala.collection.JavaConverters._
              doc
                .body()
                .children()
                .asScala
                .find { el =>
                  el.tagName() == "script" && el.html().contains("SwaggerUIBundle")
                }
                .foreach { el =>
                  val update =
                    """
const ui = SwaggerUIBundle({
  url: "/api-docs/swagger.json",
  presets: [
    SwaggerUIBundle.presets.apis,
    SwaggerUIStandalonePreset
  ],
  plugins: [
    SwaggerUIBundle.plugins.DownloadUrl
  ],
  layout: "StandaloneLayout",
  operationsSorter: "alpha"
});
window.ui = ui;"""
                  // Careful! ^ will be inserted as one-liner
                  el.text(update)
                }

              Files.createDirectories(file.getParentFile.toPath)
              IO.write(file, doc.outerHtml())
          }

          Seq(file)
        }.taskValue
      ))

  private def fileContentFromJar(jar: File, fileName: String): Option[Array[Byte]] = {
    val fs      = new BufferedInputStream(Files.newInputStream(jar.toPath))
    val factory = new ArchiveStreamFactory()
    val ais     = factory.createArchiveInputStream(fs)

    try Iterator
      .continually(ais.getNextEntry)
      .takeWhile(_ != null)
      .filter(ais.canReadEntryData)
      .find(_.getName == fileName)
      .map { _ =>
        val out = new ByteArrayOutputStream()
        IOUtils.copy(ais, out)
        out.toByteArray
      } finally fs.close()
  }
}
