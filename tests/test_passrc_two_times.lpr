{
  Copyright 2018-2020 Michalis Kamburelis.

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Test that running fcl-passrc parser multiple times is OK, and without memory leaks.
  Usage:

    # go to parent dir
    make tests/test_passrc_two_times
    cd tests/
    ./test_passrc_two_times

  TODO: Remake to be a test using fpcunit framework (see CGE tests for examples). }

{$mode objfpc}{$H+}

uses SysUtils, Classes, PParser, PasTree;

type
  { We have to override abstract TPasTreeContainer methods.
    See utils/fpdoc/dglobals.pp for an implementation of TFPDocEngine,
    a "real" engine. }
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

procedure DoIt;
var
  M: TPasModule;
  E: TPasTreeContainer;
begin
  E := TSimpleEngine.Create;
  try
    M := ParseSource(E, '-Fi/home/michalis/ dummy.pas', 'linux', 'i386');
    FreeAndNil(M);
  finally FreeAndNil(E) end;
end;

begin
  DoIt;
  DoIt;
  DoIt;
  DoIt;
end.
