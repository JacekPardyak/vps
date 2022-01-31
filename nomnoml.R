library(nomnoml)
code = "
[<actor>Customer] -> [<usecase>customeremail]
[<usecase>customeremail] -> [<actor>Agent]
[<actor>Customer] -> [<usecase>customermessage]
[<usecase>customermessage] -> [<actor>Agent]
[<actor>Agent] -> [<usecase>internalmessage]
[<usecase>internalmessage] -> [<actor>Agent] 
[<actor>Agent] -> [<usecase>messagetocustomer] 
[<usecase>messagetocustomer] -> [<actor>Customer]
"
nomnoml(code = code, png = "./img/nomnoml_0.png") 

 
    