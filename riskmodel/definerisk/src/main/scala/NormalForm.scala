package normalform.com


case class Student(id: Int, name: String)
case class Course(id: Int, name: String, professorId: Int)
case class Professor(id: Int, name: String, email: String)
case class Enrollment(studentId: Int, courseId: Int, grade: Option[String])
case class CourseProfessor(courseId: Int, professorId: Int)
case class StudentHobby(studentId: Int, hobby: String)
case class StudentLocation(studentId: Int, location: String)

def first_nf() = 
    val students: Map[Int, Student] = Map(
    1 -> Student(1, "Alice"),
    2 -> Student(2, "Bob")
    )

    val enrollments: List[(Int, String)] = List(
    (1, "Math"),
    (1, "Physics"),
    (2, "Chemistry")
    )

def second_nf() = 
    val courses: Map[Int, Course] = Map(
    101 -> Course(101, "Math", 201),
    102 -> Course(102, "Physics", 202),
    103 -> Course(103, "Chemistry", 203)
    )

    val enrollments: List[Enrollment] = List(
    Enrollment(1, 101, Some("A")),
    Enrollment(1, 102, Some("B")),
    Enrollment(2, 103, None)
    )

def third_nf() =
    val professors: Map[Int, Professor] = Map(
        201 -> Professor(201, "Dr. Smith", "smith@univ.edu"),
        202 -> Professor(202, "Dr. Brown", "brown@univ.edu"),
        203 -> Professor(203, "Dr. White", "white@univ.edu")
    )

def fourth_nf() =
    val courseProfessors: List[CourseProfessor] = List(
       CourseProfessor(101, 201),
        CourseProfessor(102, 202),
        CourseProfessor(103, 203)
    )
    val studentHobbies: Map[Int, List[String]] = Map(
        1 -> List("Chess", "Swimming"),
        2 -> List("Painting")
    )

    val hobbies: List[StudentHobby] = List(
        StudentHobby(1, "Chess"),
        StudentHobby(1, "Swimming"),
        StudentHobby(2, "Painting")
    )

def firth_nf() = 
    val studentLocations: List[StudentLocation] = List(
        StudentLocation(1, "Campus A"),
        StudentLocation(1, "Campus B"),
        StudentLocation(2, "Campus A")
    )
    