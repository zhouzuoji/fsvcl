unit FSScrollControls;

interface

uses
  SysUtils, Classes, Consts, Windows, Graphics, Controls, Messages, StdCtrls, ExtCtrls, FsGraphics, FSControls;

type
  TFsScrollContainer = class(TFsCustomControl)
  private
    FRealControl: TControl;
  protected
    function DoScroll(msg: DWORD; wparam, lparam: Integer): Integer; override;
    function GetControlScrollInfo(var si: TScrollInfo; isVert: Boolean): Boolean; override;
    function CreateRealControl: TControl; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    property RealControl: TControl read FRealControl;
  end;

  TFsHackThumbDragScrollContainer = class(TFsScrollContainer)
  protected
    procedure DragThumb(BarFlag, ScrollCode, nTrackPos: Integer); override;
  end;

implementation


type
  TControlHack = class(TControl)

  end;

{ TFsScrollContainer }

function TFsScrollContainer.DoScroll(msg: DWORD; wparam, lparam: Integer): Integer;
begin
  if Assigned(FRealControl) then
    Result := FRealControl.Perform(msg, wparam, lparam)
  else Result := -1;
end;

constructor TFsScrollContainer.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable, csPannable];

  Width := 185;
  Height := 41;

  FRealControl := Self.CreateRealControl;
  FRealControl.Parent := Self;
  FRealControl.Align := alClient;
end;

function TFsScrollContainer.GetControlScrollInfo(var si: TScrollInfo; isVert: Boolean): Boolean;
const
  BARS: array [Boolean] of DWORD = (SB_HORZ, SB_VERT);
begin
  if Assigned(FRealControl) and (FRealControl is TWinControl) and (TWinControl(FRealControl).HandleAllocated) then
  begin
    si.cbSize := SizeOf(si);
    si.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    Result := Windows.GetScrollInfo(TWinControl(FRealControl).Handle, BARS[isVert], si);
  end
  else Result := False;
end;

{ TFsHackThumbDragScrollContainer }

procedure TFsHackThumbDragScrollContainer.DragThumb(BarFlag, ScrollCode, nTrackPos: Integer);
var
  si: TScrollInfo;
  offset: Integer;
  msg: DWORD;
begin
  if BarFlag = SB_VERT then msg := WM_VSCROLL
  else msg := WM_HSCROLL;

  si.cbSize := SizeOf(si);
  si.fMask := SIF_PAGE or SIF_POS;

  if not Windows.GetScrollInfo(TWinControl(RealControl).Handle, BarFlag, si) then
  begin
    inherited;
    Exit;
  end;

  if si.nPos <> nTrackPos then
  begin
    Windows.LockWindowUpdate(Handle);

    try
      if si.nPos < nTrackPos then
      begin
        offset := nTrackPos - si.nPos;

        while offset > 0 do
        begin
          if offset >= Integer(si.nPage) then
          begin
            RealControl.Perform(msg, SB_PAGEDOWN, 0);
            Dec(offset, si.nPage);
          end
          else begin
            RealControl.Perform(msg, SB_LINEDOWN, 0);
            Dec(offset);
          end;
        end;
      end
      else begin
        offset := si.nPos - nTrackPos;

        while offset > 0 do
        begin
          if offset >= Integer(si.nPage) then
          begin
            RealControl.Perform(msg, SB_PAGEUP, 0);
            Dec(offset, si.nPage);
          end
          else begin
            RealControl.Perform(msg, SB_LINEUP, 0);
            Dec(offset);
          end;
        end;
      end;
    finally
      Windows.LockWindowUpdate(0);
      Self.Invalidate;
    end;
  end;
end;

end.
