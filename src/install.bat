@echo off

rem call build-byt.bat
rem call build.bat
setlocal

set OEBUILD=C:\Programmi\OCamlEditor\oebuild.opt.exe

%OEBUILD% datasource.ml -a -o datasource -thread -cflags "-w sy -g" -install "datasource" 
%OEBUILD% datasource.ml -a -o datasource -thread -cflags "-w sy -g" -opt -msvc -install "datasource"

%OEBUILD% postgres.ml -a -o pgdatasource -thread -cflags "-w sy -g" -I "+calendar" -l "calendar" -install "datasource" 
%OEBUILD% postgres.ml -a -o pgdatasource -thread -cflags "-w sy -g" -I "+calendar" -opt -msvc -install "datasource" 

goto exit

SET DEST="%OCAMLLIB%"\datasource
mkdir %DEST%

REM Uninstall
del %DEST%\*datasource.*
del %DEST%\*.cmi

REM Install
copy datasource.cma %DEST%
copy datasource.cmxa %DEST%
copy datasource.lib %DEST%
copy pgdatasource.cma %DEST%
copy pgdatasource.cmxa %DEST%
copy pgdatasource.lib %DEST%
rem copy sqlite3datasource.cma %DEST%
rem copy sqlite3datasource.cmxa %DEST%
rem copy sqlite3datasource.lib %DEST%
copy *.cmi %DEST%

del *.cm*
del *.lib
del *.obj

:exit
%OEBUILD% datasource.ml -clean
endlocal
