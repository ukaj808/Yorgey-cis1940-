module Party where

import Employee (Employee, GuestList(GL), empFun)


glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (empFun emp + fun) 
