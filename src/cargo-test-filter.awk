#!/usr/bin/env awk -f

/running [1-9][0-9]* test(s?)/ {
	print $0
	valid=1
}

valid > 0 { print $0 }

/test result: / {
	valid=0
}
