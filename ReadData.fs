//
// F# program to analyze payroll data.
//
// << YOUR NAME HERE >>
// U. of Illinois, Chicago
// CS 341, Spring 2020
// Project #02
//

module Project02

let doubleOrNothing s = 
    match s with
    | "" -> 0.0
    | x -> double x

// First Name, Last Name, Occupation, Dept Name, Fulltime or Part time, Typical Hours, Annual Salary, Hourly Rate
let ParseCSVLine (line:string) = 
    let tokens = line.Split(',')
    let listOfValues = Array.toList tokens
    let fName::lName::Occupation::Department::SalaryType::HoursPerWeek::AnnualSalary::HourlyWage::[] = listOfValues
    (fName,lName,Occupation,Department,SalaryType,(doubleOrNothing HoursPerWeek),(doubleOrNothing AnnualSalary),(doubleOrNothing HourlyWage))

let rec ParseCSVDatabase lines = 
    let employees = Seq.map ParseCSVLine lines
    //printfn "%A" employees
    Seq.toList employees

[<EntryPoint>]
let main argv =

  printf "Enter name of the csv file containing employee data: "

  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let data = ParseCSVDatabase contents

  printfn "This is the data you have loaded."
  List.iter (printfn "%A") data

  0    