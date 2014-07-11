args <- commandArgs(trailingOnly = TRUE)

res = read.csv(args[1], head = F)

total = res$V3 + res$V5 + res$V7 + res$V9
vuln = res$V2 + res$V4 + res$V6 + res$V8

sel1 = res$V9
det1 = res$V8
sel2 = res$V7 + res$V9
det2 = res$V6 + res$V8
sel3 = res$V5 + res$V7 + res$V9
det3 = res$V4 + res$V6 + res$V8

print(sel1)
print(det1)

det1 = det1[sel1>0]
det2 = det2[sel2>0]
det3 = det3[sel3>0]

sel1 = sel1[sel1>0]
sel2 = sel2[sel2>0]
sel3 = sel3[sel3>0]


r_vul = round(mean((100/total)*vuln), 2)
#r_sel = round(mean((100/total)*sel), 2)
#r_det = round(mean((100/vuln)*det), 2)

#print((100/sel)*det)

print(paste("Vulnerable programs from total:",r_vul))

#x = round(mean((100/sel1)*det1), 2)

#print(sel1)
#print(det1)
print(paste("Detected program from selected with 1.0 or more:",round(mean((100/sel1)*det1),2), mean(sel1)))
print(paste("Detected program from selected with 0.6 or more:",round(mean((100/sel2)*det2),2), mean(sel2)))
print(paste("Detected program from selected with 0.3 or more:",round(mean((100/sel3)*det3),2), mean(sel3)))

#x = round(mean((100/sel2)*det2), 2)
#print(x)
#x = round(mean((100/sel3)*det3), 2)
#print(x)

#print(paste("Selected programs from total:",r_sel))
#print(paste("Vulnerable programs detected from vulnerables:",r_det))
