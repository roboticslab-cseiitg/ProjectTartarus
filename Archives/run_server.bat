@echo off
echo Please press enter to run relay server on default port 8688
if "%1" == "" set /p portNumber=Enter port number:

if "%1" NEQ "" set portNumber= %1

nxjpc -jar %HOMEDRIVE%\Tartarus\interface.jar %portNumber%


	
