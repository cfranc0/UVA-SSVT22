-- Time spent: 90 minutes --

import Lecture3
import Exercise1


-- we check for each parsed form which is given in type [Form] (from the "parse" function given in the Lecture.hs) whether is the same as the normal Form
parsercheck :: [Form]->Form->Bool
parsercheck parsed formcheck= head parsed ==formcheck

-- we also check whether the parsed and the normal form, are equivalent from the function we wrote on exercise 1 
-- we wrote the function tests to combine test between multiple form and parsed forms combining the ordinary check of the Forms with the function parsercheck and the equiv function 
tests :: [Form]->[[Form]]-> Bool
tests fex1 fex2= parsercheck (head fex2) (head fex1) && equiv (head (head fex2)) (head fex1)

--here we use the example of the given forms from the Lecture3 code and their parsed representation in order to check it with our tests function
parser_tests =tests [form1, form2, form3] [parse"((1==>2)<=>(-2==>-1)) ",parse"((1==>2)<=>(-1==>-2)) " , parse"(*((1==>2) (2==>3))==>(1==>3)) "]

--We also use onother example of the given forms from the Lecture3 code and their parsed representation in order to check it with our tests function, 
-- but this time with false order in order to check the tests function 
parser_tests' =tests [form2, form2, form1] [parse"((1==>2)<=>(-2==>-1)) ",parse"((1==>2)<=>(-1==>-2)) " , parse"(*((1==>2) (2==>3))==>(1==>3)) "]
