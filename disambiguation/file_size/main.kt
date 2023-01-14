import java.io.File

fun main() {
    val file = File("testSize.json")
    val fileSizeInBytes = file.length()
    println("Kotlin: file size: $fileSizeInBytes")

    val fileContent = file.inputStream().readBytes().toString(Charsets.UTF_8)
    val bytes = fileContent.toByteArray() // Defaults to utf8
    println("Kotlin: string size: ${bytes.size}")
}
