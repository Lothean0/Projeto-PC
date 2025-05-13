@echo off
REM Change to the directory where the script is located
cd /d "%~dp0"

REM Define paths
set "SRC_DIR=Server\src"
set "BIN_DIR=out\production\Server"

REM Create the BIN_DIR directory if it doesn't exist
if not exist "%BIN_DIR%" (
    mkdir "%BIN_DIR%"
)

REM Compile all Erlang files in the source directory
echo Compiling Erlang source files...
for %%f in (%SRC_DIR%\*.erl) do (
    erlc -o "%BIN_DIR%" "%%f"
)

REM Check if compilation failed
if errorlevel 1 (
    echo Compilation failed. Exiting.
    exit /b 1
)

echo Compilation successful. Starting the server...

REM Start the Erlang VM and run the server
erl -pa "%BIN_DIR%" -noshell -eval "server:start(8000), timer:sleep(infinity)."
