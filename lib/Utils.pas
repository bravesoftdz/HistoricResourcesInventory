unit Utils;

interface

function min (a,b: integer): integer;
   overload;
function min (a,b: real): real;
   overload;
function max (a,b: integer): integer;
   overload;
function max (a,b: real): real;
   overload;

procedure CopyFile (from_file_name, to_file_name: string);

procedure RunProgram (CmdLine: String; Wait: Boolean);

function ConvertRelToAbsPath(const RelPath, BasePath: string): string;

implementation

uses Windows, SysUtils, Classes;

function min (a,b: integer): integer;
   begin
      if a < b
      then result := a
      else result := b
   end;

function min (a,b: real): real;
   begin
      if a < b
      then result := a
      else result := b
   end;

function max (a,b: integer): integer;
   begin
      if a > b
      then result := a
      else result := b
   end;

function max (a,b: real): real;
   begin
      if a > b
      then result := a
      else result := b
   end;

procedure CopyFile (from_file_name, to_file_name: string);
   var
      S, T: TFileStream;
   begin
      S := TFileStream.Create(from_file_name, fmOpenRead );
         // might throw EFOpenError if file is being modified by external editor
      try
         T := TFileStream.Create(to_file_name, fmOpenWrite or fmCreate );
         try
            T.CopyFrom(S, S.Size)
         finally
            T.Free
         end;
      finally
         S.Free
      end
   end;

procedure RunProgram (CmdLine: String; Wait: Boolean);
   var
      StartInfo: TStartupInfo;
      ProcInfo: TProcessInformation;
      CopyOfCmdLine: string;
   begin
      CopyOfCmdLine := CmdLine + ' ';
         // a separate copy is needed to ensure param is not
         //    in read-only memory (a CreateProcessW problem)

      FillChar(StartInfo,SizeOf(TStartupInfo),#0);
      FillChar(ProcInfo,SizeOf(TProcessInformation),#0);
      StartInfo.cb := SizeOf(TStartupInfo);
      if not CreateProcess (nil,
                            PChar(CopyOfCmdLine),
                            nil,
                            nil,
                            False,
                            CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS,
                            nil,
                            nil,
                            StartInfo,
                            ProcInfo
                           )
      then
         raise Exception.Create ('Unable to run ' + CmdLine);

      if Wait
      then
         WaitForSingleObject(ProcInfo.hProcess, INFINITE);

      CloseHandle(ProcInfo.hProcess);
      CloseHandle(ProcInfo.hThread)
   end;

// https://stackoverflow.com/a/5329520/4216068
function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall;
   external 'shlwapi.dll' name 'PathCanonicalizeW';

function ConvertRelToAbsPath(const RelPath, BasePath: string): string;
   var
      Dst: array[0..MAX_PATH-1] of char;
   begin
      PathCanonicalize(@Dst[0], PChar(IncludeTrailingBackslash(BasePath) + RelPath));
      result := Dst;
   end;

end.
