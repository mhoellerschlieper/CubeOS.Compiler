{*******************************************************************************
    TMemoEx clipboard utilities

    Copyright (c) 2001-2, TMemoEx.com team
    http://www.tmemoex.com, support@tmemoex.com
*******************************************************************************}

unit MemoExClipUtils;

interface

uses
  Graphics, Windows;

function _CopyToClipboard(Handle: THandle; SelText: string; FontCharset: TFontCharset): boolean;
function _PasteFromClipboard(Handle: THandle; FontCharset: TFontCharset; DeleteCRLF: boolean = true): string;

implementation

uses
  SysUtils;

var
  IsNT: boolean = false;

function _CopyToClipboard(Handle: THandle; SelText: string; FontCharset: TFontCharset): boolean;
var
  Data, LocaleData: HGLOBAL;
  _Text: PChar;
  _WText: PWideChar;
  fmt: UINT;
  size: integer;
  s: string;
  id: LCID;
  id_ptr: pointer;
begin
  Result := false;
  if SelText <> '' then
  begin
    if IsNT then size := 2 * length(SelText) + 2 else size := length(SelText) + 1;
    Data := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, size);
    if Data <> 0 then
    begin
      if IsNT then
      begin
        s := SelText;
        _WText := GlobalLock(Data);
        if FontCharset = OEM_CHARSET then OEMToChar(PChar(s), PChar(s));
        StringToWideChar(s, _WText, size);
      end
      else
        begin
          _Text := GlobalLock(Data);
          StrCopy(_Text, PChar(SelText));
        end;
      GlobalUnlock(Data);
      if IsNT then fmt := CF_UNICODETEXT
      else
        if FontCharset = OEM_CHARSET then fmt := CF_OEMTEXT else fmt := CF_TEXT;
      if OpenClipboard(Handle) then
      begin
        EmptyClipboard;
        SetClipboardData(fmt, Data);

        id := GetThreadLocale();
        LocaleData := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SizeOf(LCID));
        id_ptr := GlobalLock(LocaleData);
        Move(id, id_ptr^, SizeOf(LCID));
        SetClipboardData(CF_LOCALE, LocaleData);

        GlobalUnlock(LocaleData);
        CloseClipboard;
        Result := true;
      end
      else GlobalFree(Data);
    end;
  end;
end;

function _PasteFromClipboard(Handle: THandle; FontCharset: TFontCharset; DeleteCRLF: boolean = true): string;
var
  fmtText, fmtOEMText, fmtUnicode: boolean;
  Data: HGLOBAL;
  fmt: UINT;
  _Text: PChar;
  _WideText: PWideChar;
  Txt: string;
  i: integer;
begin
  Result := '';
  _Text := nil;
  _WideText := nil;
  fmtText := IsClipboardFormatAvailable(CF_TEXT);
  fmtOEMText := IsClipboardFormatAvailable(CF_OEMTEXT);
  fmtUnicode := (IsNT) and (IsClipboardFormatAvailable(CF_UNICODETEXT));
  if (fmtText) or (fmtOEMText) or (fmtUnicode) then
    if OpenClipboard(Handle) then
    begin
      if fmtUnicode then fmt := CF_UNICODETEXT
      else
        if (fmtOEMText) and (FontCharset = OEM_CHARSET) then fmt := CF_OEMTEXT
        else fmt := CF_TEXT;
      Data := GetClipboardData(fmt);
      if Data <> 0 then
      begin
        if fmtUnicode then _WideText := PWideChar(GlobalLock(Data))
        else _Text := GlobalLock(Data);
        if Assigned(_Text) or Assigned(_WideText) then
        begin
          if fmtUnicode then Txt := WideCharToString(_WideText)
          else Txt := StrPas(_Text);
          if (FontCharset = OEM_CHARSET) and (fmt <> CF_OEMTEXT) then CharToOEM(PChar(Txt), PChar(Txt))
          else
            if (FontCharset = DEFAULT_CHARSET) and (fmt = CF_OEMTEXT) then OEMToChar(PChar(Txt), PChar(Txt));
          if DeleteCRLF then
          begin
            i := 1;
            while i <= length(Txt) do
              if Txt[i] in [#10, #13] then System.Delete(Txt, i, 1)
              else inc(i);
          end;
          Result := Txt;
          GlobalUnlock(Data);
        end;
      end;
      CloseClipboard;
    end;
end;

initialization
  IsNT := Win32Platform = VER_PLATFORM_WIN32_NT;

end.
