#! /usr/bin/python

config_h_file_path = "wwwcount2.5/src/config.h"
makefile_config_path="../SETUP/config.txt"

import re

makefile_config = open (makefile_config_path,'r')
makefile_config_content = makefile_config.read()
makefile_config_pattern="([A-Z]*)='([^']*)'"
items = re.finditer(makefile_config_pattern, makefile_config_content)
for item in items:
    key = item.group(1)
    value = item.group(2)
    if key == "SERVERPUBLICDIR":
        config_h_file = open(config_h_file_path,'r')
        config_h_file_content = config_h_file.read()
        config_h_file_content = config_h_file_content.replace('/var/www/html/sanskrit/',value)
        break
    
