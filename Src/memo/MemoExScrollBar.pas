{*******************************************************************************
    TMemoExScrollBar
    Scroll bar with Page property for TMemoEx.

    Copyright (c) 2001-2, TMemoEx.com team
    http://www.tmemoex.com, support@tmemoex.com
*******************************************************************************}

unit MemoExScrollBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
  StdCtrls, ExtCtrls;

type
  { TMemoExScrollBar }
  TMemoExScrollBar = class
  private
    FKind: TScrollBarKind;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: TScrollBarInc;
    FLargeChange: TScrollBarInc;
    FPage : integer;
    FHandle : hWnd;
    FOnScroll: TScrollEvent;
    procedure SetParam(index, Value: Integer);
  protected
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); dynamic;
  public
    constructor Create;
    procedure SetParams(AMin, AMax, APosition, APage : integer);
    procedure DoScroll(var Message: TWMScroll);
    property Kind: TScrollBarKind read FKind write FKind default sbHorizontal;
    property SmallChange: TScrollBarInc read FSmallChange write FSmallChange default 1;
    property LargeChange: TScrollBarInc read FLargeChange write FLargeChange default 1;
    property Min  : Integer index 0 read FMin write SetParam default 0;
    property Max  : Integer index 1 read FMax write SetParam default 100;
    property Position : Integer index 2 read FPosition write SetParam default 0;
    property Page : integer index 3 read FPage write SetParam;
    property Handle : hWnd read FHandle write FHandle;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  end;

implementation

uses
  Consts, Math;

const
  SBKIND: array[TScrollBarKind] of integer = (SB_HORZ, SB_VERT);

{ TMemoExScrollBar }

constructor TMemoExScrollBar.Create;
begin
  FPage := 1;
  FSmallChange := 1;
  FLargeChange := 1;
end;

procedure TMemoExScrollBar.SetParams(AMin, AMax, APosition, APage : integer);
var
  ScrollInfo : TScrollInfo;
begin
  if AMax < AMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if APosition > Math.Max(0, AMax - FLargeChange) then APosition := AMax - FLargeChange;
  if APosition < AMin then APosition := AMin;
  if Handle > 0 then
  begin
    with ScrollInfo do
    begin
      cbSize := sizeof(TScrollInfo);
      fMask := SIF_DISABLENOSCROLL;
      if (AMin >= 0) or (AMax >= 0) then fMask := fMask or SIF_RANGE;
      if APosition >= 0 then fMask := fMask or SIF_POS;
      if APage >= 0 then fMask := fMask or SIF_PAGE;
      nPos := APosition;
      nMin := AMin;
      nMax := AMax;
      nPage := APage;
    end;
    SetScrollInfo(
      Handle,         // handle of window with scroll bar
      SBKIND[Kind] ,  // scroll bar flag
      ScrollInfo,     // pointer to structure with scroll parameters
      true            // redraw flag
    );
  end;
end;

procedure TMemoExScrollBar.SetParam(index, Value: Integer);
begin
  case index of
    0 : FMin := Value;
    1 : FMax := Value;
    2 : FPosition := Value;
    3 : FPage := Value;
  end;
  if FMax < FMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if FPosition > Math.Max(0, FMax - FLargeChange) then FPosition := FMax - FLargeChange;
  if FPosition < FMin then FPosition := FMin;
  SetParams(FMin, FMax, FPosition, FPage);
end;

procedure TMemoExScrollBar.DoScroll(var Message: TWMScroll);
var
  ScrollPos: Integer;
  NewPos: Longint;
  ScrollInfo: TScrollInfo;
begin
  with Message do
  begin
    NewPos := FPosition;
    case TScrollCode(ScrollCode) of
      scLineUp:
        Dec(NewPos, FSmallChange);
      scLineDown:
        Inc(NewPos, FSmallChange);
      scPageUp:
        Dec(NewPos, FLargeChange);
      scPageDown:
        Inc(NewPos, FLargeChange);
      scPosition, scTrack:
        with ScrollInfo do
        begin
          cbSize := SizeOf(ScrollInfo);
          fMask := SIF_ALL;
          GetScrollInfo(Handle, SBKIND[Kind], ScrollInfo);
          NewPos := nTrackPos;
        end;
      scTop:
        NewPos := FMin;
      scBottom:
        NewPos := FMax;
    end;
    if NewPos > Math.Max(0, FMax - FLargeChange) then NewPos := FMax - FLargeChange;
    if NewPos < FMin then NewPos := FMin;
    ScrollPos := NewPos;
    Scroll(TScrollCode(ScrollCode), ScrollPos);
  end;
  Position := ScrollPos;
end;

procedure TMemoExScrollBar.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then FOnScroll(Self, ScrollCode, ScrollPos);
end;

end.

