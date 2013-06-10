-export([get_employee_projects/1]).

-record(project, {name}).
-record(in_proj, {emp, proj_name}).
-record(employee, {emp_no}).

table(atom) -> [].

get_employee_projects(EmpId)->
    QH1 = [E ||
        E <- table(employee), E#employee.emp_no==EmpId,
        R <- table(in_proj),
        R#in_proj.emp == EmpId,
        R#in_proj.emp == E#employee.emp_no
    ],
    QH1.