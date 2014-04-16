unit FsControls;

interface

uses
  SysUtils, Classes, Windows, Messages, Graphics, Forms, Controls, StdCtrls, ExtCtrls, Themes, FsGraphics;

type
  TFsScrollInfo = record
    ShowVScroll: Boolean;
    ShowHScroll: Boolean;
    VScroll: TRect;
    TopArrow: TRect;
    BottomArrow: TRect;
    VThumb: TRect;
    HScroll: TRect;
    LeftArrow: TRect;
    RightArrow: TRect;
    HThumb: TRect;
    Intersect: TRect;
  end;

  TScrollHitTest = (shtNoWhere, shtBorder, shtLeftArrow, shtRightArrow, shtHorzThumb, shtPageLeft, shtPageRight,
    shtTopArrow, shtBottomArrow, shtVertThumb, shtPageUp, shtPageDown);

  TFsCustomScrollBar = class(TComponent)
  private
    FVScrollWidth: Integer;
    FHScrollHeight: Integer;
    FVArrowHeight: Integer;
    FHArrowWidth: Integer;
    FMinThumbLength: Integer;
    procedure SetHScrollHeight(const Value: Integer);
    procedure SetVScrollWidth(const Value: Integer);
    procedure SetHArrowWidth(const Value: Integer);
    procedure SetVArrowHeight(const Value: Integer);
    procedure SetMinThumbLength(const Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CalcVScroll(const rc: TRect; const si: TScrollInfo; var rcTopArrow, rcBottomArrow, rcThumb: TRect);
    procedure CalcHScroll(const rc: TRect; const si: TScrollInfo; var rcLeftArrow, rcRightArrow, rcThumb: TRect);
    function CalcVPos(const rc: TRect; const si: TScrollInfo; Y: Integer): Integer;
    function CalcHPos(const rc: TRect; const si: TScrollInfo; Y: Integer): Integer;
    procedure DrawVScroll(dc: HDC; const rc, rcTopArrow, rcBottomArrow, rcThumb: TRect); virtual; abstract;
    procedure DrawHScroll(dc: HDC; const rc, rcLeftArrow, rcRightArrow, rcThumb: TRect); virtual; abstract;
    procedure DrawIntersect(dc: HDC; const rc: TRect); virtual; abstract;
    property HScrollHeight: Integer read FHScrollHeight write SetHScrollHeight;
    property VScrollWidth: Integer read FVScrollWidth write SetVScrollWidth;
    property VArrowHeight: Integer read FVArrowHeight write SetVArrowHeight;
    property HArrowWidth: Integer read FHArrowWidth write SetHArrowWidth;
    property MinThumbLength: Integer read FMinThumbLength write SetMinThumbLength;
  end;

  TFsFlatScrollBar = class(TFsCustomScrollBar)
  private
    procedure DrawUpArrow(dc: HDC; const rc: TRect; h, bw: Integer);
    procedure DrawDownArrow(dc: HDC; const rc: TRect; h, bw: Integer);
    procedure DrawLeftArrow(dc: HDC; const rc: TRect; h, bw: Integer);
    procedure DrawRightArrow(dc: HDC; const rc: TRect; h, bw: Integer);
  public
    procedure DrawVScroll(dc: HDC; const rc, rcTopArrow, rcBottomArrow, rcThumb: TRect); override;
    procedure DrawHScroll(dc: HDC; const rc, rcLeftArrow, rcRightArrow, rcThumb: TRect); override;
    procedure DrawIntersect(dc: HDC; const rc: TRect); override;
  published
    property HScrollHeight;
    property VScrollWidth;
    property VArrowHeight;
    property HArrowWidth;
    property MinThumbLength;
  end;

  TFsGraphicControl = class(TGraphicControl)
  protected
    procedure GetContentDimension(out dim: TSize); virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
  public
    function CheckAutoSize: Boolean;
    procedure AutoSizeAndInvalidate;
  end;

  TFsCustomControl = class(TWinControl)
  private
    FCanvas: TCanvas;
    FCaptureRegion: TScrollHitTest;
    FCapturePoint: TPoint;
    FScrollBarDrawer: TFsCustomScrollBar;
    FScrollBars: TScrollStyle;
    procedure SetScrollBarDrawer(const Value: TFsCustomScrollBar);
    procedure SetScrollBars(const Value: TScrollStyle);
    function GetRealScrollBar: TFsCustomScrollBar;
    function NeedScrollBar(out HScroll: Boolean): Boolean;
    procedure GetScrollInfo(var fsi: TFsScrollInfo; var vsi, hsi: TScrollInfo; const r: TRect);
    function HitTest(X, Y: Integer): TScrollHitTest;
    procedure NCChanged;
    procedure ClipEdge(var r: TRect);
    procedure CheckScrollBarCapture(X, Y: Integer);
    procedure CheckScrollBarCaptureMouseUp(X, Y: Integer);
  protected
    FMouseInControl: Boolean;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMEraseBkgnd(var msgr: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMVScroll(var msgr: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var msgr: TWMHScroll); message WM_HSCROLL;
    procedure WMSize(var msgr: TWMSize); message WM_SIZE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMMouseWheel(var msgr: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var msgr: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCPaint(var msgr: TWMNCPaint); message WM_NCPAINT;
    procedure CMMouseEnter(var msgr: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msgr: TMessage); message CM_MOUSELEAVE;
    procedure WMNCLButtonDown(var msgr: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var msgr: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCMouseMove(var msgr: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMNCMouseHover(var msgr: TMessage); message WM_NCMOUSEHOVER;
    procedure WMNCMouseLeave(var msgr: TMessage); message WM_NCMOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DoScroll(msg: DWORD; wparam, lparam: Integer): Integer; virtual;
    function GetControlScrollInfo(var si: TScrollInfo; isVert: Boolean): Boolean; virtual;
    procedure DragThumb(BarFlag, ScrollCode, nTrackPos: Integer); virtual;
    procedure DoCanScroll(IsVert: Boolean; var nPos: Integer); dynamic;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property ScrollBarDrawer: TFsCustomScrollBar read FScrollBarDrawer write SetScrollBarDrawer;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintNC;
    procedure SetScrollRange(IsVert: Boolean; nMin, nMax, nPage: Integer);
    procedure SetScrollPos(IsVert: Boolean; nPos: Integer);
    property ParentBackground;
    property ParentCtl3D;
  end;

function NeedScroll(const si: TScrollInfo): Boolean; inline;

function GetDefaultScrollBar: TFsCustomScrollBar;

implementation

var
  ThumbTrackPos: Integer;

type
  TScrollBarTimer = class(TTimer)
  private
    procedure Scroll(Sender: TObject);
  public
    FScrollControl: TFsCustomControl;
  end;

{ TScrollBarTimer }

procedure TScrollBarTimer.Scroll(Sender: TObject);
var
  pt: TPoint;
begin
  with FScrollControl do
  begin
    case FCaptureRegion of
      shtLeftArrow: DoScroll(WM_HSCROLL, SB_LINELEFT, 0);
      shtRightArrow: DoScroll(WM_HSCROLL, SB_LINERIGHT, 0);
      shtPageLeft:
        begin
          DoScroll(WM_HSCROLL, SB_PAGELEFT, 0);
          GetCursorPos(pt);
          Windows.ScreenToClient(Handle, pt);
          if HitTest(pt.X, pt.Y) <> shtPageLeft then Self.Enabled := False;
        end;
      shtPageRight:
        begin
          DoScroll(WM_HSCROLL, SB_PAGERIGHT, 0);
          GetCursorPos(pt);
          Windows.ScreenToClient(Handle, pt);
          if HitTest(pt.X, pt.Y) <> shtPageLeft then Self.Enabled := False;
        end;
      shtTopArrow: DoScroll(WM_VSCROLL, SB_LINEUP, 0);
      shtBottomArrow: DoScroll(WM_VSCROLL, SB_LINEDOWN, 0);
      shtPageUp:
        begin
          DoScroll(WM_VSCROLL, SB_PAGEUP, 0);
          GetCursorPos(pt);
          Windows.ScreenToClient(Handle, pt);
          if HitTest(pt.X, pt.Y) <> shtPageUp then Self.Enabled := False;
        end;
      shtPageDown:
        begin
          DoScroll(WM_VSCROLL, SB_PAGEDOWN, 0);
          GetCursorPos(pt);
          Windows.ScreenToClient(Handle, pt);
          if HitTest(pt.X, pt.Y) <> shtPageDown then Self.Enabled := False;
        end;
    end;
  end;
end;

var
  DefaultScrollBar: TFsCustomScrollBar;
  ScrollBarTimer: TScrollBarTimer;

function GetDefaultScrollBar: TFsCustomScrollBar;
begin
  if not Assigned(DefaultScrollBar) then
    DefaultScrollBar := TFsFlatScrollBar.Create(nil);

  Result := DefaultScrollBar;
end;

function GetScrollBarTimer: TScrollBarTimer;
begin
  if not Assigned(ScrollBarTimer) then
  begin
    ScrollBarTimer := TScrollBarTimer.Create(nil);
    ScrollBarTimer.Enabled := False;
    ScrollBarTimer.Interval := 200;
    ScrollBarTimer.OnTimer := ScrollBarTimer.Scroll;
  end;

  Result := ScrollBarTimer;
end;

function NeedScroll(const si: TScrollInfo): Boolean; inline;
begin
  Result := (si.nPage > 0) and (si.nMin <> si.nMax) and (si.nMax - si.nMin + 1 > si.nPage);
end;

{ TFsCustomScrollBar }

const
  SCROLL_MIN_THUMB_LENGTH = 20;

function TFsCustomScrollBar.CalcHPos(const rc: TRect; const si: TScrollInfo; Y: Integer): Integer;
var
  ThumbWidth, ScrollWidth: Integer;
begin
  if (si.nMax >= si.nMin) and (si.nPage <= si.nMax - si.nMin) then
  begin
    ScrollWidth := rc.Right - rc.Left - FHArrowWidth shl 1;

    ThumbWidth := si.nPage * ScrollWidth div (si.nMax - si.nMin + 1);

    if ThumbWidth < Self.MinThumbLength then ThumbWidth := Self.MinThumbLength;

    if Y <= rc.Left + FHArrowWidth then Result := si.nMin
    else if Y > rc.Right - FHArrowWidth - ThumbWidth then Result := si.nMax - si.nPage + 1
    else begin
      Y := Y - rc.Left - FHArrowWidth;
      Result := (si.nMax - si.nMin + 1 - si.nPage) * Y div (ScrollWidth - ThumbWidth) + si.nMin - 1;
    end;
  end
  else Result := si.nMin - 1;
end;

procedure TFsCustomScrollBar.CalcHScroll(const rc: TRect; const si: TScrollInfo; var rcLeftArrow, rcRightArrow,
  rcThumb: TRect);
var
  ThumbWidth, ThumbPos, ScrollWidth: Integer;
begin
  rcLeftArrow.Left := rc.Left;
  rcLeftArrow.Right := rc.Left + FHArrowWidth;
  rcLeftArrow.Top := rc.Top;
  rcLeftArrow.Bottom := rc.Bottom;

  rcRightArrow.Left := rc.Right - FHArrowWidth;
  rcRightArrow.Right := rc.Right;
  rcRightArrow.Top := rc.Top;
  rcRightArrow.Bottom := rc.Bottom;

  if (si.nMax >= si.nMin) and (si.nPage <= si.nMax - si.nMin) then
  begin
    ScrollWidth := rc.Right - rc.Left - FHArrowWidth shl 1;

    rcThumb.Top := rc.Top;
    rcThumb.Bottom := rc.Bottom;

    ThumbWidth := si.nPage * ScrollWidth div (si.nMax - si.nMin + 1);

    if ThumbWidth < Self.MinThumbLength then ThumbWidth := Self.MinThumbLength;

    ThumbPos := (si.nPos - si.nMin) * (ScrollWidth - ThumbWidth) div (si.nMax - si.nMin + 1 - si.nPage);

    rcThumb.Left := rc.Left + FHArrowWidth + ThumbPos;
    rcThumb.Right := rcThumb.Left + ThumbWidth;
  end
  else begin
    rcThumb.Left := 0;
    rcThumb.Right := -1;
    rcThumb.Top := 0;
    rcThumb.Bottom := -1;
  end;
end;

function TFsCustomScrollBar.CalcVPos(const rc: TRect; const si: TScrollInfo; Y: Integer): Integer;
var
  ThumbHeight, ScrollHeight: Integer;
begin
  if (si.nMax >= si.nMin) and (si.nPage <= si.nMax - si.nMin) then
  begin
    ScrollHeight := rc.Bottom - rc.Top - FVArrowHeight shl 1;

    ThumbHeight := si.nPage * ScrollHeight div (si.nMax - si.nMin + 1);

    if ThumbHeight < Self.MinThumbLength then ThumbHeight := Self.MinThumbLength;

    if Y <= rc.Top + FVArrowHeight then Result := si.nMin
    else if Y > rc.Bottom - FVArrowHeight - ThumbHeight then Result := si.nMax - si.nPage + 1
    else begin
      Y := Y - rc.Top - FVArrowHeight;
      Result := (si.nMax - si.nMin + 1 - si.nPage) * Y div (ScrollHeight - ThumbHeight) + si.nMin;
    end;
  end
  else Result := si.nMin - 1;
end;

procedure TFsCustomScrollBar.CalcVScroll(const rc: TRect; const si: TScrollInfo;
  var rcTopArrow, rcBottomArrow, rcThumb: TRect);
var
  ThumbHeight, ThumbPos, ScrollHeight: Integer;
begin
  rcTopArrow.Left := rc.Left;
  rcTopArrow.Right := rc.Right;
  rcTopArrow.Top := rc.Top;
  rcTopArrow.Bottom := rcTopArrow.Top + FVArrowHeight;

  rcBottomArrow.Left := rc.Left;
  rcBottomArrow.Right := rc.Right;
  rcBottomArrow.Top := rc.Bottom - FVArrowHeight;
  rcBottomArrow.Bottom := rc.Bottom;

  if (si.nMax >= si.nMin) and (si.nPage <= si.nMax - si.nMin) then
  begin
    ScrollHeight := rc.Bottom - rc.Top - FVArrowHeight shl 1;

    rcThumb.Left := rc.Left;
    rcThumb.Right := rc.Right;

    ThumbHeight := si.nPage * ScrollHeight div (si.nMax - si.nMin + 1);

    if ThumbHeight < Self.MinThumbLength then ThumbHeight := Self.MinThumbLength;

    ThumbPos := (si.nPos - si.nMin) * (ScrollHeight - ThumbHeight) div (si.nMax - si.nMin + 1 - si.nPage);

    rcThumb.Top := rc.Top + FVArrowHeight + ThumbPos;
    rcThumb.Bottom := rcThumb.Top + ThumbHeight;
  end
  else begin
    rcThumb.Left := 0;
    rcThumb.Right := -1;
    rcThumb.Top := 0;
    rcThumb.Bottom := -1;
  end;
end;

procedure TFsCustomScrollBar.Changed;
begin

end;

constructor TFsCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FVScrollWidth := GetSystemMetrics(SM_CXVSCROLL);
  FHScrollHeight := GetSystemMetrics(SM_CYHSCROLL);
  FVArrowHeight := GetSystemMetrics(SM_CYVSCROLL);
  FHArrowWidth := GetSystemMetrics(SM_CXHSCROLL);
  FMinThumbLength := SCROLL_MIN_THUMB_LENGTH;
end;

procedure TFsCustomScrollBar.SetHArrowWidth(const Value: Integer);
begin
  if (FHArrowWidth <> Value) and (Value >= 0) then
  begin
    FHArrowWidth := Value;
    Changed;
  end;
end;

procedure TFsCustomScrollBar.SetHScrollHeight(const Value: Integer);
begin
  if (FHScrollHeight <> Value) and (Value > 0) then
  begin
    FHScrollHeight := Value;
    Changed;
  end;
end;

procedure TFsCustomScrollBar.SetMinThumbLength(const Value: Integer);
begin
  if (FMinThumbLength <> Value) and (Value >= SCROLL_MIN_THUMB_LENGTH) then
  begin
    FMinThumbLength := Value;
    Changed;
  end;
end;

procedure TFsCustomScrollBar.SetVArrowHeight(const Value: Integer);
begin
  if (FVArrowHeight <> Value) and (Value >= 0) then
  begin
    FVArrowHeight := Value;
    Changed;
  end;
end;

procedure TFsCustomScrollBar.SetVScrollWidth(const Value: Integer);
begin
  if (FVScrollWidth <> Value) and (Value > 0) then
  begin
    FVScrollWidth := Value;
    Changed;
  end;
end;

{ TFsFlatScrollBar }

procedure TFsFlatScrollBar.DrawHScroll(dc: HDC; const rc, rcLeftArrow, rcRightArrow, rcThumb: TRect);
var
  brush: HBRUSH;
begin
  Windows.FillRect(dc, rc, GetStockObject(LTGRAY_BRUSH));

  if Self.HArrowWidth > 0 then
  begin
    GradientFillRect(dc, Rect(rcLeftArrow.Left, rcLeftArrow.Top,
      rcLeftArrow.Right - 1, rcLeftArrow.Bottom),
      RGB(255, 255, 255), RGB(229, 229, 229), False);

    Self.DrawLeftArrow(dc, rcLeftArrow,
      (rcLeftArrow.Right - rcLeftArrow.Left) div 2,
      (rcLeftArrow.Bottom - rcLeftArrow.Top) div 2);

    GradientFillRect(dc, Rect(rcRightArrow.Left + 1, rcRightArrow.Top,
      rcRightArrow.Right, rcRightArrow.Bottom),
      RGB(255, 255, 255), RGB(229, 229, 229), False);

    Self.DrawRightArrow(dc, rcRightArrow,
      (rcRightArrow.Right - rcRightArrow.Left) div 2,
      (rcRightArrow.Bottom - rcRightArrow.Top) div 2);
  end;

  if rcThumb.Left < rcThumb.Right then
  begin
    brush := CreateSolidBrush(RGB(136, 136, 136));

    FillRect(dc, Rect(rcThumb.Left, rcThumb.Top + 1, rcThumb.Right, rcThumb.Top + 2), GetStockObject(WHITE_BRUSH));
    FillRect(dc, Rect(rcThumb.Left, rcThumb.Top + 2, rcThumb.Left + 1, rcThumb.Bottom - 1), GetStockObject(WHITE_BRUSH));
    FillRect(dc, Rect(rcThumb.Right - 1, rcThumb.Top + 1, rcThumb.Right, rcThumb.Bottom - 1), brush);
    FillRect(dc, Rect(rcThumb.Left + 1, rcThumb.Bottom - 2, rcThumb.Right - 1, rcThumb.Bottom - 1), brush);

    DeleteObject(brush);

    GradientFillRect(dc, Rect(rcThumb.Left + 1, rcThumb.Top + 2, rcThumb.Right - 1, rcThumb.Bottom - 2),
      RGB(254, 254, 254), RGB(229, 229, 229), True);
  end;
end;

procedure TFsFlatScrollBar.DrawVScroll(dc: HDC; const rc, rcTopArrow, rcBottomArrow, rcThumb: TRect);
var
  brush: HBRUSH;
begin
  Windows.FillRect(dc, rc, GetStockObject(LTGRAY_BRUSH));

  if VArrowHeight > 0 then
  begin
    GradientFillRect(dc, Rect(rcTopArrow.Left, rcTopArrow.Top, rcTopArrow.Right, rcTopArrow.Bottom - 1),
      RGB(255, 255, 255), RGB(229, 229, 229), False);

    Self.DrawUpArrow(dc, rcTopArrow, (rcTopArrow.Bottom - rcTopArrow.Top) div 2, (rcTopArrow.Right - rcTopArrow.Left) div 2);

    GradientFillRect(dc, Rect(rcBottomArrow.Left, rcBottomArrow.Top + 1, rcBottomArrow.Right, rcBottomArrow.Bottom),
      RGB(255, 255, 255), RGB(229, 229, 229), False);

    Self.DrawDownArrow(dc, rcBottomArrow, (rcBottomArrow.Bottom - rcBottomArrow.Top) div 2, (rcBottomArrow.Right - rcBottomArrow.Left) div 2);
  end;
  
  if rcThumb.Left < rcThumb.Right then
  begin
    brush := CreateSolidBrush(RGB(136, 136, 136));

    FillRect(dc, Rect(rcThumb.Left + 1, rcThumb.Top, rcThumb.Right - 2, rcThumb.Top + 1), GetStockObject(WHITE_BRUSH));
    FillRect(dc, Rect(rcThumb.Left + 1, rcThumb.Top + 1, rcThumb.Left + 2, rcThumb.Bottom), GetStockObject(WHITE_BRUSH));
    FillRect(dc, Rect(rcThumb.Right - 2, rcThumb.Top, rcThumb.Right - 1, rcThumb.Bottom), brush);
    FillRect(dc, Rect(rcThumb.Left + 2, rcThumb.Bottom - 1, rcThumb.Right - 1, rcThumb.Bottom), brush);

    DeleteObject(brush);

    GradientFillRect(dc, Rect(rcThumb.Left + 2, rcThumb.Top + 1, rcThumb.Right - 2, rcThumb.Bottom - 1),
      RGB(254, 254, 254), RGB(229, 229, 229), False);
  end;
end;

procedure TFsFlatScrollBar.DrawDownArrow(dc: HDC; const rc: TRect; h, bw: Integer);
var
  pts: array [0..2] of TPoint;
begin
  pts[0].X := rc.Left + (rc.Right - rc.Left - bw) div 2 ;
  pts[0].Y := rc.Top + (rc.Bottom - rc.Top - h) div 2;

  pts[1].X := pts[0].X + bw;
  pts[1].Y := pts[0].Y;

  pts[2].X := rc.Left + (rc.Right - rc.Left) div 2;
  pts[2].Y := pts[0].Y + h;

  GradientFillTriangle(dc, pts[0].X, pts[0].Y, pts[1].X, pts[1].Y, pts[2].X, pts[2].Y,
    RGB(180, 180, 180), RGB(180, 180, 180), RGB(192, 192, 192));
end;

procedure TFsFlatScrollBar.DrawIntersect(dc: HDC; const rc: TRect);
begin
  FillRect(dc, rc, GetStockObject(LTGRAY_BRUSH));
end;

procedure TFsFlatScrollBar.DrawLeftArrow(dc: HDC; const rc: TRect; h, bw: Integer);
var
  pts: array [0..2] of TPoint;
begin
  pts[0].X := rc.Left + (rc.Right - rc.Left - h) div 2;
  pts[0].Y := rc.Top + (rc.Bottom - rc.Top) div 2;

  pts[1].X := pts[0].X + h;
  pts[1].Y := rc.Top + (rc.Bottom - rc.Top - bw) div 2;

  pts[2].X := pts[1].X;
  pts[2].Y := pts[1].Y + bw;

  GradientFillTriangle(dc, pts[0].X, pts[0].Y, pts[1].X, pts[1].Y, pts[2].X, pts[2].Y,
    RGB(192, 192, 192), RGB(180, 180, 180), RGB(180, 180, 180));
end;

procedure TFsFlatScrollBar.DrawRightArrow(dc: HDC; const rc: TRect; h, bw: Integer);
var
  pts: array [0..2] of TPoint;
begin
  pts[1].X := rc.Left + (rc.Right - rc.Left - h) div 2;
  pts[1].Y := rc.Top + (rc.Bottom - rc.Top - bw) div 2;

  pts[2].X := pts[1].X;
  pts[2].Y := pts[1].Y + bw;

  pts[0].X := pts[1].X + h;
  pts[0].Y := rc.Top + (rc.Bottom - rc.Top) div 2;

  GradientFillTriangle(dc, pts[0].X, pts[0].Y, pts[1].X, pts[1].Y, pts[2].X, pts[2].Y,
    RGB(192, 192, 192), RGB(180, 180, 180), RGB(180, 180, 180));
end;

procedure TFsFlatScrollBar.DrawUpArrow(dc: HDC; const rc: TRect; h, bw: Integer);
var
  pts: array [0..2] of TPoint;
begin
  pts[0].X := rc.Left + (rc.Right - rc.Left) div 2;
  pts[0].Y := rc.Top + (rc.Bottom - rc.Top - h) div 2;

  pts[1].X := rc.Left + (rc.Right - rc.Left - bw) div 2;
  pts[1].Y := pts[0].Y + h;

  pts[2].X := pts[1].X + bw;
  pts[2].Y := pts[1].Y;

  GradientFillTriangle(dc, pts[0].X, pts[0].Y, pts[1].X, pts[1].Y, pts[2].X, pts[2].Y,
    RGB(192, 192, 192), RGB(180, 180, 180), RGB(180, 180, 180));
end;

{ TFsGraphicControl }

procedure TFsGraphicControl.AutoSizeAndInvalidate;
begin
  if not CheckAutoSize then Self.Invalidate;
end;

function TFsGraphicControl.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  dim: TSize;
begin
  Result := True;

  if not (csDestroying in ComponentState) then
  begin
    Self.GetContentDimension(dim);

    if (csDesigning in ComponentState) or (dim.cx > 0) and (dim.cy > 0) then
    begin
      if Align in [alNone, alLeft, alRight] then NewWidth := dim.cx;
      if Align in [alNone, alTop, alBottom] then NewHeight := dim.cy;
    end;
  end;
end;

function TFsGraphicControl.CheckAutoSize: Boolean;
var
  OldBounds: TRect;
begin
  if Self.AutoSize then
  begin
    OldBounds := Self.BoundsRect;

    Self.AutoSize := False;
    Self.AutoSize := True;

    Result := (OldBounds.Left <> Self.Left) or (OldBounds.Top <> Self.Top)
      or (OldBounds.Right <> Self.Left + Self.Width)
      or (OldBounds.Bottom <> Self.Top + Self.Height);

  end
  else Result := False;
end;

procedure TFsGraphicControl.GetContentDimension(out dim: TSize);
begin
  dim.cx := Self.Width;
  dim.cy := Self.Height;
end;

{ TFsCustomControl }

procedure TFsCustomControl.CheckScrollBarCapture(X, Y: Integer);
var
  pt: TPoint;
  fsi: TFsScrollInfo;
  vsi, hsi: TScrollInfo;
  nPos: Integer;
  r: TRect;
begin
  case FCaptureRegion of
    shtLeftArrow, shtRightArrow, shtPageLeft, shtPageRight,
    shtTopArrow, shtBottomArrow, shtPageUp, shtPageDown:
      begin
        GetCursorPos(pt);
        Windows.ScreenToClient(Handle, pt);
        GetScrollBarTimer.Enabled := HitTest(pt.X, pt.Y) = FCaptureRegion;
      end;

    shtVertThumb:
      begin
        Self.ClipEdge(r);
        Self.GetScrollInfo(fsi, vsi, hsi, r);

        if fsi.ShowVScroll then
        begin
          nPos := GetRealScrollBar.CalcVPos(fsi.VScroll, vsi, Y - FCapturePoint.Y);

          if nPos >= hsi.nMin then
            Self.DragThumb(SB_VERT, SB_THUMBTRACK, nPos);
        end;
      end;

    shtHorzThumb:
      begin
        Self.ClipEdge(r);
        Self.GetScrollInfo(fsi, vsi, hsi, r);

        if fsi.ShowHScroll then
        begin
          nPos := GetRealScrollBar.CalcHPos(fsi.HScroll, hsi, X - FCapturePoint.X);

          if nPos >= hsi.nMin then
            Self.DragThumb(SB_HORZ, SB_THUMBTRACK, nPos);
        end;
      end;
  end;
end;

procedure TFsCustomControl.CheckScrollBarCaptureMouseUp(X, Y: Integer);
var
  fsi: TFsScrollInfo;
  vsi, hsi: TScrollInfo;
  nPos: Integer;
  r: TRect;
begin
  try
    case FCaptureRegion of
      shtVertThumb:
        begin
          Self.ClipEdge(r);
          Self.GetScrollInfo(fsi, vsi, hsi, r);

          if fsi.ShowVScroll then
          begin
            nPos := GetRealScrollBar.CalcVPos(fsi.VScroll, vsi, Y - FCapturePoint.Y);

            if nPos >= hsi.nMin then
              Self.DragThumb(SB_VERT, SB_THUMBPOSITION, nPos);
          end;
        end;

      shtHorzThumb:
        begin
          Self.ClipEdge(r);
          Self.GetScrollInfo(fsi, vsi, hsi, r);

          if fsi.ShowHScroll then
          begin
            nPos := GetRealScrollBar.CalcHPos(fsi.HScroll, hsi, X - FCapturePoint.X);

            if nPos >= hsi.nMin then
              Self.DragThumb(SB_HORZ, SB_THUMBPOSITION, nPos);
          end;
        end;
    end;
  finally
    ReleaseCapture;
    FCaptureRegion := shtNoWhere;
    GetScrollBarTimer.Enabled := False;
  end;
end;

procedure TFsCustomControl.ClipEdge(var r: TRect);
var
  cxEdge, cyEdge: Integer;
begin
  GetWindowRect(Handle, r);
  Windows.MapWindowPoints(0, Handle, r, 2);
  
  if BevelKind <> bkNone then
  begin
    cxEdge := GetSystemMetrics(SM_CXEDGE) shr 1;
    cyEdge := GetSystemMetrics(SM_CYEDGE) shr 1;

    if BevelInner <> bvNone then
    begin
      if beLeft in BevelEdges then Inc(r.Left, cxEdge);
      if beTop in BevelEdges then Inc(r.Top, cyEdge);
      if beRight in BevelEdges then Dec(r.Right, cxEdge);
      if beBottom in BevelEdges then Dec(r.Bottom, cyEdge);
    end;

    if BevelOuter <> bvNone then
    begin
      if beLeft in BevelEdges then Inc(r.Left, cxEdge);
      if beTop in BevelEdges then Inc(r.Top, cyEdge);
      if beRight in BevelEdges then Dec(r.Right, cxEdge);
      if beBottom in BevelEdges then Dec(r.Bottom, cyEdge);
    end;
  end;
end;

procedure TFsCustomControl.CMMouseEnter(var msgr: TMessage);
begin
  inherited;
  FMouseInControl := True;
  Self.Invalidate;
end;

procedure TFsCustomControl.CMMouseLeave(var msgr: TMessage);
begin
  inherited;
  FMouseInControl := False;
  Self.Invalidate;
end;

constructor TFsCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := True;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  BevelKind := bkFlat;
  BevelInner := bvNone;
  BevelOuter := bvRaised;
  BevelEdges := [beLeft, beTop, beRight, beBottom];
  FScrollBars := ssNone;
end;

procedure TFsCustomControl.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := Params.Style and not WS_BORDER;
  Params.WindowClass.style := Params.WindowClass.style or CS_HREDRAW or CS_VREDRAW;
end;

destructor TFsCustomControl.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TFsCustomControl.DragThumb(BarFlag, ScrollCode, nTrackPos: Integer);
begin
  ThumbTrackPos := nTrackPos;
  
  if BarFlag = SB_VERT then
    Self.DoScroll(WM_VSCROLL, MakeLong(ScrollCode, nTrackPos), 0)
  else
    Self.DoScroll(WM_HSCROLL, MakeLong(ScrollCode, nTrackPos), 0);
end;

function TFsCustomControl.GetControlScrollInfo(var si: TScrollInfo; isVert: Boolean): Boolean;
const
  BARS: array [Boolean] of DWORD = (SB_HORZ, SB_VERT);
begin
  si.cbSize := SizeOf(si);
  si.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
  Result := Self.HandleAllocated and Windows.GetScrollInfo(Self.Handle, BARS[isVert], si);
end;

function TFsCustomControl.GetRealScrollBar: TFsCustomScrollBar;
begin
  if Assigned(FScrollBarDrawer) then Result := FScrollBarDrawer
  else Result := GetDefaultScrollBar;
end;

procedure TFsCustomControl.GetScrollInfo(var fsi: TFsScrollInfo; var vsi, hsi: TScrollInfo; const r: TRect);
var
  sb: TFsCustomScrollBar;
begin
  sb := GetRealScrollBar;

  fsi.ShowVScroll := GetControlScrollInfo(vsi, True) and NeedScroll(vsi);

  fsi.ShowHScroll := GetControlScrollInfo(hsi, False) and NeedScroll(hsi);

  if fsi.ShowVScroll then
  begin
    fsi.VScroll.Right := r.Right;
    fsi.VScroll.Left := fsi.VScroll.Right - sb.VScrollWidth;
    fsi.VScroll.Top := r.Top;

    if fsi.ShowHScroll then fsi.VScroll.Bottom := r.Bottom - sb.HScrollHeight
    else fsi.VScroll.Bottom := r.Bottom;

    sb.CalcVScroll(fsi.VScroll, vsi, fsi.TopArrow, fsi.BottomArrow, fsi.VThumb);
  end;

  if fsi.ShowHScroll then
  begin
    fsi.HScroll.Bottom := r.Bottom;
    fsi.HScroll.Top := fsi.HScroll.Bottom - sb.HScrollHeight;
    fsi.HScroll.Left := r.Left;

    if fsi.ShowHScroll then fsi.HScroll.Right := r.Right - sb.VScrollWidth
    else fsi.HScroll.Right := r.Right;

    sb.CalcHScroll(fsi.HScroll, hsi, fsi.LeftArrow, fsi.RightArrow, fsi.HThumb);
  end;

  if fsi.ShowVScroll and fsi.ShowHScroll then
  begin
    fsi.Intersect.Left := r.Right - sb.VScrollWidth;
    fsi.Intersect.Right := r.Right;
    fsi.Intersect.Top := r.Bottom - sb.HScrollHeight;
    fsi.Intersect.Bottom := r.Bottom;
  end;
end;

procedure TFsCustomControl.DoCanScroll(IsVert: Boolean; var nPos: Integer);
begin

end;

function TFsCustomControl.DoScroll(msg: DWORD; wparam, lparam: Integer): Integer;
begin
  Result := Self.Perform(msg, wparam, lparam);
end;

function TFsCustomControl.HitTest(X, Y: Integer): TScrollHitTest;
var
  fsi: TFsScrollInfo;
  vsi, hsi: TScrollInfo;
  pt: TPoint;
  r: TRect;
begin
  pt.X := X;
  pt.Y := Y;

  Self.ClipEdge(r);

  Self.GetScrollInfo(fsi, vsi, hsi, r);

  Result := shtNoWhere;

  if fsi.ShowVScroll then
  begin
    if PtInRect(fsi.TopArrow, pt) then Result := shtTopArrow
    else if PtInRect(fsi.BottomArrow, pt) then Result := shtBottomArrow
    else if PtInRect(fsi.VThumb, pt) then Result := shtVertThumb
    else if PtInRect(fsi.VScroll, pt) then
    begin
      if pt.Y < fsi.VThumb.Top then Result := shtPageUp
      else Result := shtPageDown;
    end;
  end;

  if fsi.ShowHScroll and (Result = shtNoWhere) then
  begin
    if PtInRect(fsi.LeftArrow, pt) then Result := shtLeftArrow
    else if PtInRect(fsi.RightArrow, pt) then Result := shtRightArrow
    else if PtInRect(fsi.HThumb, pt) then Result := shtHorzThumb
    else if PtInRect(fsi.HScroll, pt) then
    begin
      if pt.X < fsi.HThumb.Left then Result := shtPageLeft
      else Result := shtPageRight;
    end;
  end;
end;

procedure TFsCustomControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (csDesigning in ComponentState) and (CanFocus or (GetParentForm(Self) = nil)) then
  begin
    SetFocus;
  end;
  
  inherited;
end;

procedure TFsCustomControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FCaptureRegion <> shtNoWhere then Self.CheckScrollBarCapture(X, Y)
  else inherited;
end;

procedure TFsCustomControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FCaptureRegion <> shtNoWhere then
    Self.CheckScrollBarCaptureMouseUp(X, Y)
  else
    inherited;
end;

procedure TFsCustomControl.NCChanged;
begin
  if HandleAllocated then
  begin
    SetWindowPos(Handle, 0, 0,0,0,0, SWP_NOACTIVATE or
    SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED);

    if Visible then Self.PaintNC;
  end;
end;

function TFsCustomControl.NeedScrollBar(out HScroll: Boolean): Boolean;
var
  si: TScrollInfo;
begin
  HScroll := GetControlScrollInfo(si, False) and NeedScroll(si);
  Result := GetControlScrollInfo(si, True) and NeedScroll(si);
end;

procedure TFsCustomControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (AComponent = FScrollBarDrawer) and (Operation = opRemove) then
  begin
    FScrollBarDrawer := nil;
    Self.NCChanged;
  end;
end;

procedure TFsCustomControl.Paint;
begin

end;

procedure TFsCustomControl.PaintNC;
const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  dc: HDC;
  rw: TRect;
  sb: TFsCustomScrollBar;
  fsi: TFsScrollInfo;
  vsi, hsi: TScrollInfo;
begin
  rw.Left := 0;
  rw.Top := 0;
  rw.Right := Width;
  rw.Bottom := Height;
  
  dc := GetWindowDC(Handle);
  
  try
    if BevelKind <> bkNone then
      Windows.DrawEdge(dc, rw, InnerStyles[BevelInner] or OuterStyles[BevelOuter],
        Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[Ctl3D] or BF_ADJUST);

    sb := GetRealScrollBar;

    Self.GetScrollInfo(fsi, vsi, hsi, rw);

    if fsi.ShowVScroll then
    begin
      Dec(rw.Right, sb.VScrollWidth);
      sb.DrawVScroll(dc, fsi.VScroll, fsi.TopArrow, fsi.BottomArrow, fsi.VThumb);
    end;

    if fsi.ShowHScroll then
    begin
      Dec(rw.Bottom, sb.HScrollHeight);
      sb.DrawHScroll(dc, fsi.HScroll, fsi.LeftArrow, fsi.RightArrow, fsi.HThumb);
    end;

    if fsi.ShowVScroll and fsi.ShowHScroll then
      sb.DrawIntersect(dc, fsi.Intersect);
  finally
    ReleaseDC(Handle, dc);
  end;
end;

procedure TFsCustomControl.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TFsCustomControl.SetScrollBarDrawer(const Value: TFsCustomScrollBar);
begin
  if Value <> FScrollBarDrawer then
  begin
    if Assigned(FScrollBarDrawer) then
      FScrollBarDrawer.RemoveFreeNotification(Self);

    FScrollBarDrawer := Value;

    if Assigned(FScrollBarDrawer) then
      FScrollBarDrawer.FreeNotification(Self);

    Self.NCChanged;
  end;
end;

procedure TFsCustomControl.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    Self.NCChanged;  
  end;
end;

procedure TFsCustomControl.SetScrollPos(IsVert: Boolean; nPos: Integer);
const
  BAR_FLAGS: array [Boolean] of DWORD = (SB_HORZ, SB_VERT);
begin
  Windows.SetScrollPos(Handle, BAR_FLAGS[IsVert], nPos, True);
end;

procedure TFsCustomControl.SetScrollRange(IsVert: Boolean; nMin, nMax, nPage: Integer);
const
  BAR_FLAGS: array [Boolean] of DWORD = (SB_HORZ, SB_VERT);
var
  si: TScrollInfo;
begin
  si.cbSize := SizeOf(si);
  si.fMask := SIF_RANGE or SIF_PAGE;
  si.nMin := nMin;
  si.nMax := nMax;
  si.nPage := nPage;
  Windows.SetScrollInfo(Handle, BAR_FLAGS[IsVert], si, True);
  Self.NCChanged;
end;

procedure TFsCustomControl.WMCancelMode(var Message: TWMCancelMode);
begin
  inherited;

  FCaptureRegion := shtNoWhere;
end;

procedure TFsCustomControl.WMEraseBkgnd(var msgr: TWMEraseBkgnd);
begin
  if ParentBackground and Assigned(Parent) then
  begin
    if Parent.DoubleBuffered then PerformEraseBackground(Self, msgr.DC)
    else ThemeServices.DrawParentBackground(Handle, msgr.DC, nil, False);
  end
  else if not FDoubleBuffered or (msgr.DC = HDC(msgr.Unused)) then
    Windows.FillRect(msgr.DC, ClientRect, Self.Brush.Handle);

  msgr.Result := 1;
end;

procedure TFsCustomControl.WMHScroll(var msgr: TWMHScroll);
var
  si: TScrollInfo;
  nPos: Integer;
begin
  Self.GetControlScrollInfo(si, False);
  nPos := si.nPos;

  case msgr.ScrollCode of
    SB_LINELEFT: Dec(nPos);
    SB_LINERIGHT: Inc(nPos);
    SB_PAGELEFT: Dec(nPos, si.nPage);
    SB_PAGERIGHT: Inc(nPos, si.nPage);
    SB_THUMBTRACK, SB_THUMBPOSITION: nPos := msgr.Pos;
    SB_LEFT: nPos := si.nMin;
    SB_RIGHT: nPos := si.nMax - si.nPage + 1;
  end;

  if nPos < si.nMin then nPos := si.nMin;

  if nPos > si.nMax - si.nPage + 1 then nPos := si.nMax - si.nPage + 1;

  DoCanScroll(True, nPos);

  if nPos <> si.nPos then
  begin
    Windows.SetScrollPos(Handle, SB_HORZ, nPos, False);
    Self.PaintNC;
  end;
end;

procedure TFsCustomControl.WMMouseWheel(var msgr: TWMMouseWheel);
var
  ShiftState: TShiftState;
  m, nPos: Integer;
  si: TScrollInfo;
begin
  ShiftState := KeysToShiftState(msgr.Keys);

  WheelAccumulator := WheelAccumulator + msgr.WheelDelta;

  m := WheelAccumulator div WHEEL_DELTA;

  if (m <> 0) and Self.GetControlScrollInfo(si, True) and NeedScroll(si) then
  begin
    nPos := si.nPos;
    nPos := nPos - m;

    if nPos < si.nMin then nPos := si.nMin;

    if nPos > si.nMax - si.nPage + 1 then nPos := si.nMax - si.nPage + 1;

    if nPos <> si.nPos then
      Self.DragThumb(SB_VERT, SB_THUMBPOSITION, nPos);
  end;

  WheelAccumulator := WheelAccumulator mod WHEEL_DELTA;
end;

procedure TFsCustomControl.WMNCCalcSize(var msgr: TWMNCCalcSize);
var
  cxEdge, cyEdge: Integer;
  r: PRect;
  sb: TFsCustomScrollBar;
  fv, fh: Boolean;
begin
  r := @msgr.CalcSize_Params.rgrc[0];

  if BevelKind <> bkNone then
  begin
    cxEdge := GetSystemMetrics(SM_CXEDGE) shr 1;
    cyEdge := GetSystemMetrics(SM_CYEDGE) shr 1;

    if BevelInner <> bvNone then
    begin
      if beLeft in BevelEdges then Inc(r.Left, cxEdge);
      if beTop in BevelEdges then Inc(r.Top, cyEdge);
      if beRight in BevelEdges then Dec(r.Right, cxEdge);
      if beBottom in BevelEdges then Dec(r.Bottom, cyEdge);
    end;

    if BevelOuter <> bvNone then
    begin
      if beLeft in BevelEdges then Inc(r.Left, cxEdge);
      if beTop in BevelEdges then Inc(r.Top, cyEdge);
      if beRight in BevelEdges then Dec(r.Right, cxEdge);
      if beBottom in BevelEdges then Dec(r.Bottom, cyEdge);
    end;
  end;

  sb := Self.GetRealScrollBar;

  fv := Self.NeedScrollBar(fh);

  if fv then Dec(r.Right, sb.VScrollWidth);

  if fh then Dec(r.Bottom, sb.HScrollHeight);

  msgr.Result := 0;
end;

procedure TFsCustomControl.WMNCHitTest(var msgr: TWMNCHitTest);
var
  pt: TPoint;
  rc: TRect;
  HitTest: TScrollHitTest;
begin
  pt.X := msgr.XPos;
  pt.Y := msgr.YPos;
  Windows.ScreenToClient(Handle, pt);
  Windows.GetClientRect(Handle, rc);

  msgr.Result := HTCLIENT;
  HitTest := Self.HitTest(pt.X, pt.Y);

  case HitTest of
    shtNoWhere: if not PtInRect(rc, pt) then msgr.Result := HTBORDER;
    shtBorder: msgr.Result := HTBORDER;
    shtLeftArrow, shtRightArrow, shtHorzThumb, shtPageLeft, shtPageRight: msgr.Result := HTHSCROLL;
    shtTopArrow, shtBottomArrow, shtVertThumb, shtPageUp, shtPageDown: msgr.Result := HTVSCROLL;
  end;
end;

procedure TFsCustomControl.WMNCLButtonDown(var msgr: TWMNCLButtonDown);
var
  pt: TPoint;
  sht: TScrollHitTest;
  DoCapture: Boolean;
  fsi: TFsScrollInfo;
  vsi, hsi: TScrollInfo;
  r: TRect;
begin
  pt.X := msgr.XCursor;
  pt.Y := msgr.YCursor;
  Windows.ScreenToClient(Handle, pt);

  DoCapture := True;
  sht := Self.HitTest(pt.X, pt.Y);

  case sht of
    shtNoWhere, shtBorder: DoCapture := False;
    shtLeftArrow: Self.DoScroll(WM_HSCROLL, SB_LINELEFT, 0);
    shtRightArrow: Self.DoScroll(WM_HSCROLL, SB_LINERIGHT, 0);
    shtHorzThumb: ;
    shtPageLeft: Self.DoScroll(WM_HSCROLL, SB_PAGELEFT, 0);
    shtPageRight: Self.DoScroll(WM_HSCROLL, SB_PAGERIGHT, 0);
    shtTopArrow: Self.DoScroll(WM_VSCROLL, SB_LINEUP, 0);
    shtBottomArrow: Self.DoScroll(WM_VSCROLL, SB_LINEDOWN, 0);
    shtVertThumb: ;
    shtPageUp: Self.DoScroll(WM_VSCROLL, SB_PAGEUP, 0);
    shtPageDown: Self.DoScroll(WM_VSCROLL, SB_PAGEDOWN, 0);
  end;

  if DoCapture then
  begin
    FCaptureRegion := sht;
    SetCapture(Self.Handle);

    if FCaptureRegion in [shtVertThumb, shtHorzThumb] then
    begin
      Self.ClipEdge(r);
      Self.GetScrollInfo(fsi, vsi, hsi, r);
      FCapturePoint.X := pt.X - fsi.HThumb.Left;
      FCapturePoint.Y := pt.Y - fsi.VThumb.Top;
    end
    else begin
      GetScrollBarTimer.FScrollControl := Self;
      GetScrollBarTimer.Enabled := True;
    end;
  end;
end;

procedure TFsCustomControl.WMNCLButtonUp(var msgr: TWMNCLButtonUp);
var
  pt: TPoint;
begin
  pt.X := msgr.XCursor;
  pt.Y := msgr.YCursor;
  Windows.ScreenToClient(Handle, pt);
  
  if FCaptureRegion <> shtNoWhere then
    Self.CheckScrollBarCaptureMouseUp(pt.X, pt.Y)
  else
    inherited;
end;

procedure TFsCustomControl.WMNCMouseHover(var msgr: TMessage);
begin
  inherited;
end;

procedure TFsCustomControl.WMNCMouseLeave(var msgr: TMessage);
begin
  inherited;
end;

procedure TFsCustomControl.WMNCMouseMove(var msgr: TWMNCMouseMove);
begin
  inherited;
end;

procedure TFsCustomControl.WMNCPaint(var msgr: TWMNCPaint);
begin
  try
    PaintNC;
  except

  end;
  msgr.Result := 0;
end;

procedure TFsCustomControl.WMPaint(var Message: TWMPaint);
begin
  Self.ControlState := Self.ControlState + [csCustomPaint];
  inherited;
  Self.ControlState := Self.ControlState - [csCustomPaint];
end;

procedure TFsCustomControl.WMSize(var msgr: TWMSize);
begin
  inherited;
  NCChanged;
end;

procedure TFsCustomControl.WMVScroll(var msgr: TWMVScroll);
var
  si: TScrollInfo;
  nPos: Integer;
begin
  Self.GetControlScrollInfo(si, True);
  nPos := si.nPos;

  case msgr.ScrollCode of
    SB_LINEUP: Dec(nPos);
    SB_LINEDOWN: Inc(nPos);
    SB_PAGEUP: Dec(nPos, si.nPage);
    SB_PAGEDOWN: Inc(nPos, si.nPage);
    SB_THUMBTRACK, SB_THUMBPOSITION: nPos := msgr.Pos;
    SB_TOP: nPos := si.nMin;
    SB_BOTTOM: nPos := si.nMax - si.nPage + 1;
  end;

  if nPos < si.nMin then nPos := si.nMin;

  if nPos > si.nMax - si.nPage + 1 then nPos := si.nMax - si.nPage + 1;

  DoCanScroll(True, nPos);

  if nPos <> si.nPos then
  begin
    Windows.SetScrollPos(Handle, SB_VERT, nPos, True);
    Self.PaintNC;
  end;
end;

initialization

finalization
  DefaultScrollBar.Free;
  ScrollBarTimer.Free;

end.
