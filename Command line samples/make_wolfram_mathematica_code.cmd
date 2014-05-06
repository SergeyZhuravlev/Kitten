::cd ..\Kitten\bin\Debug
::chcp 20866
Kitten /line-quality 3,0 /image-source "Kitten.jpg" /save-generated-code-to-file "Kitten.txt" /prologue "generated_list%20%3D%20Drop%5B%7B%0A" /epilogue "%7D%2C%20-1%5D%0A" /code-generate-pattern "%7B%24%23%7Bx%7D%2C%20%24%23%7By%7D%7D%2C%0A"
pause
exit


rem prologue:
generated_list = Drop[{

rem epilogue:
}, -1]

rem code-generate-pattern:
{$#{x}, $#{y}},
