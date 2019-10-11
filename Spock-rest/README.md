# Spock-rest

A simple API built with Spock that allows a user to POST to the `addEmployee` path with a json body to add an employee to a sqlite3 database. The json object should look like:
```
{
"name":"John"
"age":"30"
"emailAddress":"john@email.com"
}
```
The endpoint will first verify that the json is formatted correctly and will then check if each of the fields are valid. If so, it will be added to the database and will received a json object specifying the employee's id. If the fields are not valid, the user will receive the appropriate error message.

The user can then send a GET request to the `employees` path, placing the employee's id in the query parameters to receive a json representation of that employee if it is found.

###### Examples
```
curl -H "Content-Type: application/json" -X POST -d "{\"name\":\"John\",\"age\":\"30\",\"emailAddress\":\"john@email.com\"}" http:localhost:8080/addEmployee
```
```
curl -H "Content-Type: application/json" -X GET http://localhost:8080/employees/1
```
