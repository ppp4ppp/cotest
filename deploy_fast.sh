
ssh fsnadmin@192.168.200.145 pkill -9 cotest
stack exec -- whereis cotest-exe | awk '{ system("scp " $2 " fsnadmin@192.168.200.145:/home/fsnadmin/") }'
