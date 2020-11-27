@echo off
echo Staring Tartarus platform

if EXIST "%programfiles%\swipl" set mynewpath="%programfiles%\swipl\bin\swipl-win.exe"

if EXIST "%programfiles(x86)%\swipl" set mynewpath="%programfiles(x86)%\swipl\bin\swipl-win.exe"

START "swipl" %mynewpath% -s init.pl -g tart_start
