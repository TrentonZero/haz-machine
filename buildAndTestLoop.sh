while (true) do
runghc Setup clean &> /dev/null
runghc Setup configure --enable-tests &> /dev/null
runghc Setup build &> /dev/null
date &> build.log
runghc Setup test >> build.log 2>&1
clear
cat build.log | ./hilite.sh -f green PASS | ./hilite.sh -f red FAIL | ./hilite.sh -f green Pass | ./hilite.sh -f red Fail
read -t 3 -p "Press Ctrl-C to break"
done
