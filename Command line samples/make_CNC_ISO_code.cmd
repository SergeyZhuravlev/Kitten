::cd ..\Kitten\bin\Debug
::chcp 20866
Kitten /line-quality 3,0 /image-source "Kitten.jpg" /save-generated-code-to-file "Kitten.iso" /item-number-multiplier 10 /item-number-shift 100 /prologue "%25%0A%3A100%20T2%20G90%20G94%0AN10%20S0.05%20M3%0AN20%20F50%0AN30%20G30%0A" /epilogue "N70000%20M5%0AN80000%20M30%0A" /code-generate-pattern "N%24%23%7Bnumber%7D%20X%24%23%7Bx%7D%20Y%24%23%7By%7D%0A"
pause
exit


rem prologue:
%
:100 T2 G90 G94
N10 S0.05 M3
N20 F50
N30 G30

rem epilogue:
N70000 M5
N80000 M30

rem code-generate-pattern:
N$#{number} X$#{x} Y$#{y}