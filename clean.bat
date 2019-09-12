@echo off
del *~
rem del *.rst
rem del *.lrt
rem del *.lps
del /s *.or
del /s *.bak
del /s *.exe
del /s *.ppu
del /s *.o
del *.compiled
del /s /q lib\*
rmdir /s /q lib\
rmdir /s /q backup\
rmdir /s /q source\backup
rmdir /s /q source\carik\backup
rmdir /s /q source\facebook\backup
rmdir /s /q source\telegram\backup
rmdir /s /q source\line\backup
rmdir /s /q source\botframework\backup

del /s /q source\carik\lib\*
del /s /q source\facebook\lib\*
del /s /q source\telegram\lib\*
del /s /q source\line\lib\*
del /s /q source\botframework\lib\*

del /s /q public_html\carik\*.bin

timeout /t 3
