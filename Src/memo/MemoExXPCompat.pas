{*******************************************************************************
    Windows XP themes support for TMemoEx.

    Copyright (c) 2001-2, TMemoEx.com team
    http://www.tmemoex.com, support@tmemoex.com
*******************************************************************************}

unit MemoExXPCompat;

{$MINENUMSIZE 4}

interface

uses
  Windows;

type
  HTHEME = THandle;

  TIsThemeActive = function: boolean; stdcall;
  TOpenThemeData = function (hwnd: HWND; pszClassList: PWideChar): HTHEME; stdcall;
  TCloseThemeData = function (Theme: HTHEME) : HRESULT; stdcall;
  TIsThemeBackgroundPartiallyTransparent = function(Theme: HTHEME; iPartId, iStateId: integer): boolean; stdcall;
  TDrawThemeParentBackground = function (hwnd: HWND; hdc: HDC; const Rect: TRect): HRESULT; stdcall;
  TDrawThemeBackground = function (Theme: HTHEME; hdc: HDC; iPartId: integer; iStateId: integer; const Rect: TRect; pClipRect: PRect): HRESULT; stdcall;

const
  WM_THEMECHANGED = $031A;

  EP_EDITTEXT   = 1;
  EP_CARET      = 2;

  ETS_NORMAL    = 1;
  ETS_HOT       = 2;
  ETS_SELECTED  = 3;
  ETS_DISABLED  = 4;
  ETS_FOCUSED   = 5;
  ETS_READONLY  = 6;
  ETS_ASSIST    = 7;

var
  ThemesAvailable: boolean;
  IsThemeActive: TIsThemeActive;
  OpenThemeData: TOpenThemeData;
  CloseThemeData: TCloseThemeData;
  DrawThemeParentBackground: TDrawThemeParentBackground;
  DrawThemeBackground: TDrawThemeBackground;
  IsThemeBackgroundPartiallyTransparent: TIsThemeBackgroundPartiallyTransparent;

function CreateThemeHandle(wnd: HWND): HTHEME;
procedure FreeThemeHandle(var theme: HTHEME);

implementation

uses
  SysUtils;

const
  themelib = 'uxtheme.dll';
  ThemedClassName: WideString = 'EDIT';

var
  hThemeLib: HINST;

function CreateThemeHandle(wnd: HWND): HTHEME;
begin
  if (ThemesAvailable) and (IsThemeActive) then Result := OpenThemeData(wnd, PWideChar(ThemedClassName))
  else Result := 0;
end;

procedure FreeThemeHandle(var theme: HTHEME);
begin
  if (ThemesAvailable) and (theme > 0) then CloseThemeData(theme);
end;

initialization
  if (Win32Platform  = VER_PLATFORM_WIN32_NT) and
     (((Win32MajorVersion = 5) and (Win32MinorVersion >= 1)) or
      (Win32MajorVersion > 5)) then
  begin
    hThemeLib := LoadLibrary(themelib);
    if hThemeLib <> 0 then
    begin
      IsThemeActive := GetProcAddress(hThemeLib, 'IsThemeActive');
      OpenThemeData := GetProcAddress(hThemeLib, 'OpenThemeData');
      CloseThemeData := GetProcAddress(hThemeLib, 'CloseThemeData');
      DrawThemeParentBackground := GetProcAddress(hThemeLib, 'DrawThemeParentBackground');
      DrawThemeBackground := GetProcAddress(hThemeLib, 'DrawThemeBackground');
      IsThemeBackgroundPartiallyTransparent := GetProcAddress(hThemeLib, 'IsThemeBackgroundPartiallyTransparent');
      ThemesAvailable := True;
    end;
  end;

finalization
  ThemesAvailable := false;

end.

