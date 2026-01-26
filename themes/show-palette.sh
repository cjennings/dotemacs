#!/bin/bash
# Display dupre theme color palette in terminal

echo "Dupre Theme Palette"
echo "==================="
echo ""
echo "=== Neutrals (60%) ==="
printf '\e[38;2;21;19;17m██████\e[0m bg       #151311\n'
printf '\e[38;2;37;35;33m██████\e[0m bg+1     #252321\n'
printf '\e[38;2;71;69;68m██████\e[0m bg+2     #474544\n'
printf '\e[38;2;88;87;78m██████\e[0m gray-2   #58574e\n'
printf '\e[38;2;108;106;96m██████\e[0m gray-1   #6c6a60\n'
printf '\e[38;2;150;147;133m██████\e[0m gray     #969385\n'
printf '\e[38;2;180;177;162m██████\e[0m gray+1   #b4b1a2\n'
printf '\e[38;2;208;203;192m██████\e[0m gray+2   #d0cbc0\n'
printf '\e[38;2;240;254;240m██████\e[0m fg       #f0fef0\n'
echo ""
echo "=== Cool Neutrals (steel) ==="
printf '\e[38;2;138;148;150m██████\e[0m steel    #8a9496\n'
printf '\e[38;2;172;176;179m██████\e[0m steel+1  #acb0b3\n'
printf '\e[38;2;192;199;202m██████\e[0m steel+2  #c0c7ca\n'
echo ""
echo "=== Signature Blue (30%) ==="
printf '\e[38;2;103;128;156m██████\e[0m blue     #67809c  ← SIGNATURE\n'
printf '\e[38;2;178;195;204m██████\e[0m blue+1   #b2c3cc\n'
printf '\e[38;2;217;226;255m██████\e[0m blue+2   #d9e2ff\n'
echo ""
echo "=== Accents (10% total) ==="
printf '\e[38;2;215;175;95m██████\e[0m yellow   #d7af5f  ← muted gold\n'
printf '\e[38;2;255;215;95m██████\e[0m yellow+1 #ffd75f\n'
printf '\e[38;2;135;95;0m██████\e[0m yellow-2 #875f00\n'
echo ""
printf '\e[38;2;164;172;100m██████\e[0m green    #a4ac64\n'
printf '\e[38;2;204;199;104m██████\e[0m green+1  #ccc768\n'
printf '\e[38;2;100;109;20m██████\e[0m green-2  #646d14  ← mossy\n'
echo ""
printf '\e[38;2;212;124;89m██████\e[0m red      #d47c59\n'
printf '\e[38;2;167;80;45m██████\e[0m red-1    #a7502d  ← terracotta\n'
printf '\e[38;2;124;42;9m██████\e[0m red-2    #7c2a09  ← warm terracotta\n'
printf '\e[38;2;255;42;0m██████\e[0m intense  #ff2a00  ← errors only\n'
