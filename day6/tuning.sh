#!/bin/bash

first_match=$(grep -P '([a-z])(?!\1)([a-z])(?!\1|\2)([a-z])(?!\1|\2|\3)([a-z])' input.txt -ob | head -n 1)
ind=${first_match%:????} # first_match will look like "3:jqdm", so we cut off the colon and four undesired characters
echo $((ind + 4))