import java.io.{BufferedInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import Dependencies.Version
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
          val jarName       = s"swagger-ui-${Version.swaggerUi}.jar"
          val indexHtmlPath = s"META-INF/resources/webjars/swagger-ui/${Version.swaggerUi}/index.html"
          val outputFile    = resourceManaged.value / indexHtmlPath

          val html = (Compile / dependencyClasspath).value
            .find(_.data.getName == jarName)
            .flatMap(jar => fileContentFromJar(jar.data, indexHtmlPath))
            .map { new String(_, StandardCharsets.UTF_8) }

          val resource = s"$jarName:$indexHtmlPath"
          html match {
            case None => throw new RuntimeException(s"Can't find $resource")
            case Some(html) =>
              val doc = org.jsoup.parser.Parser.parse(html, "127.0.0.1")
              import scala.jdk.CollectionConverters._
              doc
                .body()
                .children()
                .asScala
                .find { el =>
                  el.tagName() == "script" && el.html().contains("SwaggerUIBundle")
                } match {
                case None => throw new RuntimeException("Can't patch script in index.html")
                case Some(el) =>
                  val update =
                    """
const ui = SwaggerUIBundle({
    url: "/api-docs/swagger.json",
    dom_id: '#swagger-ui',
    deepLinking: true,
    presets: [ SwaggerUIBundle.presets.apis ],
    plugins: [ SwaggerUIBundle.plugins.DownloadUrl ],
    layout: "BaseLayout",
    operationsSorter: "alpha"
});
window.ui = ui;
"""
                  // Careful! ^ will be inserted as one-liner
                  el.text(update)
              }

              Files.createDirectories(outputFile.getParentFile.toPath)
              IO.write(outputFile, doc.outerHtml())
          }

          Seq(outputFile)
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
