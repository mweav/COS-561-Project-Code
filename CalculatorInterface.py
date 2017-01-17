import sys

ans = 0;


msglist = []

tm = "\n\ttransMonitor "
rinr = tm + "(Recv Inr)"
sinr = tm + "(Send Inr)"
rinl = tm + "(Recv Inl)"
sinl = tm + "(Send Inl)"
rend = tm + "(Recv End)"
send = tm + "(Send End)"

def recv1nat(n=None):
    if n is None:
        sys.stdout.write("\nEnter a number ")
        n = int(raw_input())
    for i in xrange(n):
        msglist.append(rinr)
    msglist.append(rinl)
    msglist.append(rend)
    return n
    
def send1nat(n):
    for i in xrange(n):
        msglist.append(sinr)
    msglist.append(sinl)
    msglist.append(send)

func_code = """
witness_calc :: State (Maybe St) Bool
witness_calc = do
\tinitMonitor calculator"""


terminate = False
while(not terminate):
    print "\nmenu"
    b = int(raw_input())
    if (b==0):
        msglist.append(rinr)
        msglist.append(send)
        terminate = True
    else:
        msglist.append(rinl)
        recv1nat(b)
        if (b==1): #add1
            msglist.append(sinl)
            msglist.append(sinl)
            msglist.append(sinl)
            n1 = recv1nat()
            ans = ans + n1
            send1nat(ans)
        elif (b==2): #add2
            msglist.append(rinl)
            msglist.append(sinl)
            msglist.append(sinl)
            msglist.append(sinr)
            n1 = recv1nat()
            n2 = recv1nat()
            ans = n1 + n2
            send1nat(ans)
        elif (b==3): #mul1
            msglist.append(rinl)
            msglist.append(sinl)
            msglist.append(sinr)
            msglist.append(sinl)
            n1 = recv1nat()
            ans = ans * n1
            send1nat(ans)
        elif (b==4): #mul2
            msglist.append(rinl)
            msglist.append(sinl)
            msglist.append(sinr)
            msglist.append(sinr)
            n1 = recv1nat()
            n2 = recv1nat()
            ans = n1 * n2
            send1nat(ans)
        elif (b==5): #exp1
            msglist.append(rinl)
            msglist.append(sinr)
            msglist.append(sinl)
            n1 = recv1nat()
            ans = pow(ans,n1)
            send1nat(ans)
        elif (b==6): #exp2
            msglist.append(rinl)
            msglist.append(sinr)
            msglist.append(sinr)
            n1 = recv1nat()
            n2 = recv1nat()
            ans = pow(n1,n2)
            send1nat(ans)
        else:
            sys.exit(1) #Eventually move this to the Haskell part.
    print ("ANSWER = " + str(ans))
    
func_code = func_code + "".join(msglist) + "\n\nsuccess_calc = witness_calc >> isSuccess"
print "\n\n" + func_code
    
