
library(PROF)
# to generate the parameter structure for a single disease
par_list1 = init_par_list(diseases=c("covid19"),
                         models=c("seirh"))
str(par_list1)

# generate a multi-disease parameter structure
par_list2 = init_par_list(diseases=c("covid19", "influenza"),
                         models=c("seirh", "sirh"))
str(par_list2)

# write the par_list to a YAML file
write_par_list_yaml(par_list=par_list2, 
                    file_path="~/Dropbox/CSMB03/jamie/test.yml")

# read a parameter list from a YAML file
read_list = read_par_list_yaml(file_path="~/Dropbox/CSMB03/jamie/test.yml")

# double-check that the parameter list has not changed
identical(par_list2, read_list)
