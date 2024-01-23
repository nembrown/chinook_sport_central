@echo off
if exist "C:\Program Files\R\R-3.0.1\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.0.1\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.1.0\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.1.0\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.1.2\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.1.2\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.1.3\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.1.3\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.2.0\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.2.0\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.2.2\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.2.3\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.2.3\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.2.4\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.2.4\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.2.5\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.2.5\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.3.2\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.3.2\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.4.3\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.4.3\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.5.0\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.5.0\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.5.1\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.5.1\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-3.5.2\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-3.5.2\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-4.0.2\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-4.0.2\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-4.1.0\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-4.1.0\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-4.2\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-4.1.0\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-4.2.0\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-4.2.0\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-4.2.1\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-4.2.1\bin\x64\Rscript.exe"
if exist "C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe" set R_HOME="C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"
cd /D "%~dp0"
echo %R_HOME%

start "Fetch Sport Data" %R_HOME% Sport_odbc_test.R

pause