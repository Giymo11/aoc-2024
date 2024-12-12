//> using dep com.lihaoyi::os-lib::0.11.3

import os._

def write_to_file(path: Path, filename: String, content: String): Unit =
  val actual_path = path / filename
  if (os.exists(actual_path)) {
    println(s"Error: File '$filename' already exists!")
    return
  }
  os.write(actual_path, content)
  println(s"$filename created successfully!")

@main def generate_today(day: Int, test_input: String): Unit =
  val template_path = os.pwd / "Day.sc.template"
  val filename = s"day$day.sc"

  if (!os.exists(template_path)) {
    println("Error: Template file 'dayTemplate.sc' not found!")
    return
  }
  val content = os.read(template_path).replace("{{day}}", day.toString)

  write_to_file(os.pwd / "src", filename, content)
  write_to_file(os.pwd / "input", s"test$day.txt", test_input)
