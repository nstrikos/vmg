{
appsettings.pas

Class which stores the program settings

Copyright (C) 1998 - 2007 Harri Pyy, Chris O'Donnell, Felipe Monteiro de Carvalho

This file is part of Virtual Magnifying Glass.

Virtual Magnifying Glass is free software;
you can redistribute it and/or modify it under the
terms of the GNU General Public License version 2
as published by the Free Software Foundation.

Virtual Magnifying Glass is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

Please note that the General Public License version 2 does not permit
incorporating Virtual Magnifying Glass into proprietary
programs.

AUTHORS: Chris O'Donnell, Felipe Monteiro de Carvalho and Harri Pyy
}
unit appsettings;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

{$IFDEF Win32}
  {$DEFINE Windows}
{$ENDIF}

interface

uses
{$IFDEF Windows}
  Windows, shlobj,
{$ENDIF}
  Classes, SysUtils, Forms, IniFiles, translationsvmg, constants;

type
  { TConfigurations }

  TConfigurations = class(TObject)
  private
    ConfigFilePath: string;
  public
    iMagnification: Double;
    iMagnificationMax: Integer;
    xSquare, ySquare: Integer;
    xSquareMax, ySquareMax: Integer;
    showTools, graphicalBorder, AntiAliasing: Boolean;
    showWhenExecuted, autoRun: Boolean;
    invertColors: Boolean;
    MyDirectory: string;
    Language: Integer;
    firstRun: Boolean;
    // Hot Key
    HotKeyMode: Integer;
    HotKeyKey: Char;
    // Plugins
    UsePlugins: Boolean;
    PluginName: string;
    PluginData: Integer;
    UseRemap: Boolean;
    RemapWidth: Integer;
    RemapHeight: Integer;
    AntiAliasingMode : string;
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromFile(Sender: TObject);
    procedure Save(Sender: TObject);
    function GetConfigFilePath: string;
    function GetMyDirectory: string;
    function GetSystemLanguage: Integer;
    function GetOSVersion: TOSVersion;
  end;

var
  vConfigurations: TConfigurations;

implementation

{$ifdef Darwin}
uses
  MacOSAll;
{$endif}

{ TConfigurations }

{*******************************************************************
*  TConfigurations.Create ()
*
*  DESCRIPTION:    Creates an object,
*                  populates the class with the default configurations and
*                  then tryes to load the configurations from the XML file
*
*                  Under Mac OS X there is also the need to find the location
*                  of the Application Bundle
*
*  PARAMETERS:     None
*
*  RETURNS:        A pointer to the newly created object
*
*******************************************************************}
constructor TConfigurations.Create;
begin
  { First we use some good defaults in case the configuration file doesn't yet exist }

  iMagnification := 2.0;
  xSquare := 16;
  ySquare := 10;
  showTools := False;
  graphicalBorder := True;
  AntiAliasing := False;
  showWhenExecuted := True;
  autoRun := False;
  invertColors := False;
  Language := GetSystemLanguage();
  HotKeyMode := 1;
  HotKeyKey := 'm';
  UsePlugins := False;
  PluginName := 'pas_overlays.dll';
  PluginData := -1;

  UseRemap := False;
  RemapWidth := 50;
  RemapHeight := 50;
  AntiAliasingMode := IdentSimpleAntiAliasing;

  { Now identifies where the configuration file should be }
  ConfigFilePath := GetConfigFilePath();

  // Under Mac OS X we need to get the location of the bundle
  MyDirectory := GetMyDirectory;

  ReadFromFile(nil);

  vTranslations.TranslateToLanguage(Language);
end;

{*******************************************************************
*  TConfigurations.Destroy ()
*
*  DESCRIPTION:    Dont call this method directly. Use Free instead.
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
destructor TConfigurations.Destroy;
begin
  Save(nil);

  inherited Destroy;
end;

{*******************************************************************
*  TConfigurations.ReadFromFile ()
*
*  DESCRIPTION:
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TConfigurations.ReadFromFile(Sender: TObject);
var
  MyFile: TIniFile;
begin
  if not FileExists(ConfigFilePath) then Exit;

  MyFile := TIniFile.Create(ConfigFilePath);
  try
    xSquare := MyFile.ReadInteger(SectionGeneral, IdentxSquare, 16);
    ySquare := MyFile.ReadInteger(SectionGeneral, IdentySquare, 10);
    iMagnification := MyFile.ReadFloat(SectionGeneral, IdentiMagnification, 2.0);
    showTools := MyFile.ReadBool(SectionGeneral, IdentTools, False);
    graphicalBorder := MyFile.ReadBool(SectionGeneral, IdentgraphicalBorder, True);
    invertColors := MyFile.ReadBool(SectionGeneral, IdentInvertColors, False);
    AntiAliasing := MyFile.ReadBool(SectionGeneral, IdentAntiAliasing, False);
    showWhenExecuted := MyFile.ReadBool(SectionGeneral, IdentShowWhenExecuted, True);
    autoRun := MyFile.ReadBool(SectionGeneral, IdentAutoRun, False);
    Language := MyFile.ReadInteger(SectionGeneral, IdentLanguage, ID_MENU_ENGLISH);
    firstRun := MyFile.ReadBool(SectionGeneral, IdentFirstRun, True);

    {$ifdef UNIX}
      {$ifndef DARWIN}
        MyDirectory := MyFile.ReadString(SectionUnix, IdentMyDirectory, DefaultDirectory);
      {$endif}
    {$endif}

    HotKeyMode := MyFile.ReadInteger(SectionWindows, IdentHotKeyMode, ID_HOTKEY_CTRLALTKEY);
    HotKeyKey := MyFile.ReadString(SectionWindows, IdentHotKeyKey, ID_DEFAULT_HOTKEY)[1];

    UsePlugins := MyFile.ReadBool(SectionPlugins, IdentUsePlugins, False);
    PluginName := MyFile.ReadString(SectionPlugins, IdentPluginName, 'pas_overlays.dll');
    PluginData := MyFile.ReadInteger(SectionPlugins, IdentPluginData, 0);

    UseRemap := MyFile.ReadBool(SectionAdditionalEffects, IdentUseRemap, False);
    RemapWidth := MyFile.ReadInteger(SectionAdditionalEffects, IdentRemapWidth, 50);
    RemapHeight := MyFile.ReadInteger(SectionAdditionalEffects, IdentRemapHeight, 50);

    AntiAliasingMode := MyFile.ReadString(SectionAdditionalEffects, IdentAntiAliasingMode, IdentSimpleAntiAliasing);
  finally
    MyFile.Free;

    {*******************************************************************
    *  Fix for the default value apparently not working on Windows for Float
    *******************************************************************}
    if iMagnification = 0.0 then iMagnification := 2.0;
  end;
end;

{*******************************************************************
*  TConfigurations.Save ()
*
*  DESCRIPTION:
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TConfigurations.Save(Sender: TObject);
var
  MyFile: TIniFile;
begin
  MyFile := TIniFile.Create(ConfigFilePath);
  try
    MyFile.WriteInteger(SectionGeneral, IdentxSquare, xSquare);
    MyFile.WriteInteger(SectionGeneral, IdentySquare, ySquare);
    MyFile.WriteFloat(SectionGeneral, IdentiMagnification, iMagnification);
    MyFile.WriteBool(SectionGeneral, IdentTools, showTools);
    MyFile.WriteBool(SectionGeneral, IdentgraphicalBorder, graphicalBorder);
    MyFile.WriteBool(SectionGeneral, IdentInvertColors, invertColors);
    MyFile.WriteBool(SectionGeneral, IdentAntiAliasing, AntiAliasing);
    MyFile.WriteBool(SectionGeneral, IdentShowWhenExecuted, showWhenExecuted);
    MyFile.WriteBool(SectionGeneral, IdentAutoRun, autoRun);
    MyFile.WriteInteger(SectionGeneral, IdentLanguage, Language);
    MyFile.WriteBool(SectionGeneral, IdentFirstRun, firstRun);

    MyFile.WriteString(SectionUnix, IdentMyDirectory, MyDirectory);

    MyFile.WriteInteger(SectionWindows, IdentHotKeyMode, HotKeyMode);
    MyFile.WriteString(SectionWindows, IdentHotKeyKey, HotKeyKey);

    MyFile.WriteBool(SectionPlugins, IdentUsePlugins, UsePlugins);
    MyFile.WriteString(SectionPlugins, IdentPluginName, PluginName);
    MyFile.WriteInteger(SectionPlugins, IdentPluginData, PluginData);

    MyFile.WriteBool(SectionAdditionalEffects, IdentUseRemap, UseRemap);
    MyFile.WriteInteger(SectionAdditionalEffects, IdentRemapWidth, RemapWidth);
    MyFile.WriteInteger(SectionAdditionalEffects, IdentRemapHeight, RemapHeight);

    Myfile.WriteString(SectionAdditionalEffects, IdentAntiAliasingMode, AntiAliasingMode);
  finally
    MyFile.Free;
  end;
end;

function TConfigurations.GetConfigFilePath: string;
var
  APath : Array[0..MAX_PATH] of char;
  LocalConfigFile: string;
  attrs: Integer;
begin
{$ifdef Windows}

  // First tryes to use a configuration file in the application directory
  // Necessary to use the magnifier as a "portable" application
  LocalConfigFile := ExtractFilePath(Application.EXEName) + 'magnifier.ini';

  // Only uses the file if it exist, isn't readonly and isn't a directory
  attrs := FileGetAttr(LocalConfigFile);

  if FileExists(LocalConfigFile) and (attrs and faReadOnly = 0)
   and (attrs and faDirectory = 0) then
  begin
    Result := LocalConfigFile;
  end
  else
  begin
    // GetAppConfigFile from FPC is too unstable to be used
    // Microsoft recomends CSIDL_APPDATA\Company\Product\Version\

    Result := '';

    SHGetSpecialFolderPath(0, APath, CSIDL_APPDATA, True);

//  Another option: uses shfolder  SHGetFolderPath(0, CSIDL_APPDATA or CSIDL_FLAG_CREATE,0,0,@APATH[0]);

    Result := IncludeTrailingPathDelimiter(StrPas(@APath[0])) + 'Magnifier\';

    ForceDirectories(Result);

    Result := Result + 'magnifier.ini';
  end;
{$endif}
{$ifdef Unix}
  Result := GetEnvironmentVariable('HOME') + '/.magnifier.ini';
{$endif}
end;

function TConfigurations.GetMyDirectory: string;
{$ifdef Darwin}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$endif}
begin
{$ifdef UNIX}
{$ifdef Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourcesDirectory;
{$else}
  Result := DefaultDirectory;
{$endif}
{$endif}

{$ifdef Windows}
  Result := ExtractFilePath(Application.EXEName);
{$endif}
end;

function TConfigurations.GetSystemLanguage: Integer;
  {$ifdef Windows}
var
  LangId: Word;
  MainLangId: Byte;
begin
  LangId := GetSystemDefaultLangID;
  MainLangId := LangId;
  case MainLangId of

    LANG_PORTUGUESE: Result := ID_MENU_PORTUGUESE;
    LANG_SPANISH: Result := ID_MENU_SPANISH;
    LANG_FRENCH: Result := ID_MENU_FRENCH;
    LANG_GERMAN: Result := ID_MENU_GERMAN;
    LANG_ITALIAN: Result := ID_MENU_ITALIAN;
    LANG_RUSSIAN: Result := ID_MENU_RUSSIAN;
    LANG_POLISH: Result := ID_MENU_POLISH;
    LANG_JAPANESE: Result := ID_MENU_JAPANESE;

  else
    Result := ID_MENU_ENGLISH;
  end;
end;
  {$endif}
  {$ifdef UNIX}
begin
  Result := ID_MENU_ENGLISH;
end;
  {$endif}

{@@
  Returns the Windows Version
}
function TConfigurations.GetOSVersion: TOSVersion;
{$IFDEF Windows}
var
  Versao: TOSVersionInfo;
  WinMajorVersion, WinMinorVersion: Cardinal;
begin
  Result := vwDesconhecida;

  {----------------------------------------------
    Inicializations
   ----------------------------------------------}
  Versao.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if not GetVersionEx(Versao) then Exit;
  WinMajorVersion := Versao.dwMajorVersion;
  WinMinorVersion := Versao.dwMinorVersion;

  {----------------------------------------------
    Identifyes the Operating System
   ----------------------------------------------
    Series 9x
   ----------------------------------------------}
  if Versao.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
  begin
    if (WinMajorVersion = 4) and
     (WinMinorVersion = 90) then
     Result := vwWinMe
    else if (WinMajorVersion = 4) and
     (WinMinorVersion = 10) then
     Result := vwWin98
    else Result := vwWin95;
  end
  {----------------------------------------------
    Series NT
   ----------------------------------------------}
  else if Versao.dwPlatformId = VER_PLATFORM_WIN32_NT then
  begin
    if (WinMajorVersion = 3) and
     (WinMinorVersion = 10) then
     Result := vwWinNT351
    else if (WinMajorVersion = 4) then
     Result := vwWinNT
    else if (WinMajorVersion = 5) and (WinMinorVersion = 0) then
     Result := vwWin2000
    else if (WinMajorVersion = 5) and (WinMinorVersion = 1) then
     Result := vwWinXP
    else if (WinMajorVersion = 5) and (WinMinorVersion = 2) then
     Result := vwWin2003
    else if (WinMajorVersion >= 6) then
     Result := vwWinVista;
  end
  {----------------------------------------------
    Windows CE
   ----------------------------------------------}
  else if Versao.dwPlatformId = VER_PLATFORM_WIN32_CE then
  begin
    case WinMajorVersion of
     1: Result := vwWinCE1;
     2: Result := vwWinCE2;
     3: Result := vwWinCE3;
     4: Result := vwWinCE4;
    else
     Result := vwWinCE5;
    end;
  end;
end;
{$ELSE}
begin
  Result := vwDesconhecida;
end;
{$ENDIF}

{*******************************************************************
*  Initialization section
*
*  DESCRIPTION:    Upon startup an instance of the TConfigurations object
*                  is created to make the configuration info available for
*                  TGlass.
*
*******************************************************************}
initialization

  vConfigurations := TConfigurations.Create;

{*******************************************************************
*  Finalization section
*
*  DESCRIPTION:    Free memory allocated on the initialization section
*
*******************************************************************}
finalization

  FreeAndNil(vConfigurations);

end.
