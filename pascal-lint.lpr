{
  Copyright 2016-2020 Michalis Kamburelis.

  This file is part of "pascal-lint".

  "pascal-lint" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "pascal-lint" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "pascal-lint"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Parse files using fcl-passrc.
  TODO: perform tests described in README.md }

{$mode objfpc}{$H+}{$J-}

uses SysUtils, Classes, PParser, PasTree;

type
  { Simplest fcl-passrc engine.
    See http://wiki.freepascal.org/fcl-passrc . }
  TSimpleEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

function TSimpleEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  { dummy implementation, see TFPDocEngine.FindElement for a real example }
  Result := nil;
end;

var
  InputFiles: TStringList;
  EngineOptions: TStringList;
  Verbose: boolean = false;

procedure ParseParameters;

  { Is Prefix the prefix of S. Case-sensitive. }
  function IsPrefix(const Prefix, S: string): boolean;
  begin
    Result := AnsiCompareStr(Copy(S, 1, Length(Prefix)), Prefix) = 0;
  end;

var
  I: Integer;
  AllRemainingOptionsAreFiles: boolean;
begin
  AllRemainingOptionsAreFiles := false;
  for I := 1 to ParamCount do
  begin
    if AllRemainingOptionsAreFiles then
    begin
      InputFiles.Add(ParamStr(I));
    end else
    if ParamStr(I) = '--help' then
    begin
      Writeln(
        'Pascal lint (code analysis for potential errors).' + LineEnding +
        '' + LineEnding +
        'Usage: simply pass a list of parseable Pascal files (units, programs etc.)' + LineEnding +
        'as parameters, for example:' + LineEnding +
        '' + LineEnding +
        '  pascal-lint myunit.pas' + LineEnding +
        '  pascal-lint *.pas' + LineEnding +
        '  pascal-lint `find -iname *.pas`' + LineEnding +
        '' + LineEnding +
        'Additional options:' + LineEnding +
        '  --help    Show this help' + LineEnding +
        '  --verbose Be verbose' + LineEnding +
        '  --        All remaining options are files (useful if filenames may start with dash)' + LineEnding +
        '' + LineEnding +
        '  The rest of options is passed to the fcl-passrc engine,' + LineEnding +
        '  many FPC syntax options are allowed, like:' + LineEnding +
        '  -Sc       C-like operators (+=)' + LineEnding +
        '  -Mxxx     Mode (like -Mobjfpc or -Mdelphi)' + LineEnding +
        '  -Fixxx    Add a directory to seach include files' + LineEnding +
        '' + LineEnding +
        'This software is a free and open-source software.' + LineEnding +
        'Copyright Michalis Kamburelis.' + LineEnding +
        'See https://github.com/michaliskambi/pascal-lint');
      Halt;
    end else
    if ParamStr(I) = '--' then
      AllRemainingOptionsAreFiles := true
    else
    if ParamStr(I) = '--verbose' then
      Verbose := true
    else
    { at the end, if option unhandled, pass to EngineOptions }
    if IsPrefix('-', ParamStr(I)) then
      EngineOptions.Add(ParamStr(I))
    else
    { at the end, if not an option, it's a filename }
      InputFiles.Add(ParamStr(I));
  end;
end;

type
  TDynamicStringArray = array of string;

  TStringsHelper = class helper for TStrings
    { Convert TStrings to a dynamic string array. }
    function ToArray: TDynamicStringArray;
  end;

function TStringsHelper.ToArray: TDynamicStringArray;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
end;

procedure CheckFile(const FileName: string);
var
  M: TPasModule;
  E: TPasTreeContainer;
  NewEngineOptions: TStringList;
begin
  E := TSimpleEngine.Create;
  try
    try
      NewEngineOptions := TStringList.Create;
      try
        NewEngineOptions.Assign(EngineOptions);
        NewEngineOptions.Add(FileName);
        if Verbose then
          Writeln(ErrOutput, 'Executing ' + EngineOptions.Text);
        M := ParseSource(E, NewEngineOptions.ToArray, { any OS, architecture } 'linux', 'x86_64', []);
      finally FreeAndNil(NewEngineOptions) end;
    except
      on Ex: EParserError do
      begin
        Writeln(ErrOutput, 'Warning: cannot parse ' + FileName + '. If the source compiles (with any compiler, FPC or Delphi), please submit a bugreport about the fcl-passrc to http://bugs.freepascal.org/ .');
        Writeln(ErrOutput, Ex.ClassName + ': ' + Ex.Message);
        Exit;
      end;
    end;

    Writeln(ErrOutput, 'Checking ' + FileName);
    FreeAndNil(M);
  finally FreeAndNil(E) end;
end;

var
  I: Integer;
begin
  InputFiles := TStringList.Create;
  try
    EngineOptions := TStringList.Create;
    try
      ParseParameters;
      if InputFiles.Count = 0 then
        raise Exception.Create('No files to parse specified');
      Writeln(ErrOutput, 'Checking ', InputFiles.Count, ' files');
      for I := 0 to InputFiles.Count - 1 do
        CheckFile(InputFiles[I]);
    finally FreeAndNil(EngineOptions) end;
  finally FreeAndNil(InputFiles) end;
end.
