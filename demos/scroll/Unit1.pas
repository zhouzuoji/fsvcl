unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Grids,
  Dialogs, FSGraphics, FSStdCtrls, StdCtrls, ExtCtrls, ComCtrls, FSScrollControls, FSVclBase, FSNavTree,
  FsControls;

type
  TForm1 = class(TForm)
    npdMinButton: TFsNinePitchDrawable;
    npdTopBorder: TFsNinePitchDrawable;
    npdClose: TFsNinePitchDrawable;
    npdBottomBorder: TFsNinePitchDrawable;
    npdLeftBorder: TFsNinePitchDrawable;
    npdMaxButton: TFsNinePitchDrawable;
    FsImage1: TFsImage;
    npdRightBorder: TFsNinePitchDrawable;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    FsMemo1: TFsMemo;
    TabSheet2: TTabSheet;
    FsListBox1: TFsListBox;
    TabSheet3: TTabSheet;
    FsListView1: TFsListView;
    TabSheet4: TTabSheet;
    FsTreeView1: TFsTreeView;
    FsFlatScrollBar1: TFsFlatScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FNCCanvas: TCanvas;
    FMinBtnMouseDown: Boolean;
    FMinBtnMouseOver: Boolean;
    FMaxBtnMouseDown: Boolean;
    FMaxBtnMouseOver: Boolean;
    FCloseBtnMouseDown: Boolean;
    FCloseBtnMouseOver: Boolean;
    FCaptureMouseArea: Integer;
    procedure PaintNC;
    procedure PaintButtons(PaintMin, PaintMax, PaintClose: Boolean);
    function GetMinBtnRect: TRect;
    function GetMaxBtnRect: TRect;
    function GetCloseBtnRect: TRect;
  protected
    procedure WMMouseMove(var msgr: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var msgr: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMNCPaint(var msgr: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCMouseMove(var msgr: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMNCMouseLeave(var msgr: TWMNCMouseMove); message WM_NCMOUSELEAVE;
    procedure WMNCLButtonDown(var msgr: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var msgr: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCActivate(var msgr: TWMNCActivate); message WM_NCACTIVATE;
    procedure WMNCHitTest(var msgr: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
  public
    procedure InitListView;
    procedure InitTreeView;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  TITLE_BAR_HEIGHT: Integer = 32;
  LEFT_BORDER_WIDTH: Integer = 4;
  RIGHT_BORDER_WIDTH: Integer = 4;
  BOTTOM_BORDER_WIDTH: Integer = 4;
  BORDER_ICON_WIDTH: Integer = 24;
  BORDER_ICON_HEIGHT: Integer = 24;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TITLE_BAR_HEIGHT := npdTopBorder.Height;
  LEFT_BORDER_WIDTH := npdLeftBorder.Width;
  RIGHT_BORDER_WIDTH := npdRightBorder.Width;
  BOTTOM_BORDER_WIDTH := npdBottomBorder.Height;

  FNCCanvas := TCanvas.Create;

  InitListView;
  InitTreeView;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FNCCanvas.Free;
end;

function TForm1.GetCloseBtnRect: TRect;
begin
  Result.Top := (npdTopBorder.Height - npdClose.Height) div 2;
  Result.Left := Self.Width - npdClose.Width - 2;
  Result.Right := Result.Left + npdClose.Width;
  Result.Bottom := Result.Top + npdClose.Height;
end;

function TForm1.GetMaxBtnRect: TRect;
begin
  Result.Top := (npdTopBorder.Height - npdMaxButton.Height) div 2;
  Result.Left := Self.Width - npdClose.Width - 2 - npdMaxButton.Width;
  Result.Right := Result.Left + npdMaxButton.Width;
  Result.Bottom := Result.Top + npdMaxButton.Height;
end;

function TForm1.GetMinBtnRect: TRect;
begin
  Result.Top := (npdTopBorder.Height - npdMinButton.Height) div 2;
  Result.Left := Self.Width - npdClose.Width - 2 - npdMaxButton.Width - npdMinButton.Width;
  Result.Right := Result.Left + npdMinButton.Width;
  Result.Bottom := Result.Top + npdMinButton.Height;
end;

procedure TForm1.InitListView;
var
  i, j: Integer;
  li: TListItem;
begin
  FsListView1.Columns.Clear;

  for i := 1 to 20 do
    FsListView1.Columns.Add.Caption := 'column' + IntToStr(i);

  FsListBox1.Items.BeginUpdate;

  try
    for i := 1 to 1000 do
    begin
      li := FsListView1.Items.Add;
      li.Caption := 'No' + IntToStr(i);

      for j := 2 to 20 do
        li.SubItems.Add('row' + IntToStr(i) + ', col' + IntToStr(j));
    end;
  finally
    FsListBox1.Items.EndUpdate;
  end;
end;

procedure TForm1.InitTreeView;
var
  i, j: Integer;
  node: TTreeNode;
begin
  for i := 1 to 100 do
  begin
    node := FsTreeView1.Items.Add(nil, 'Level0_' + IntToStr(i));

    for j := 1 to 10 do
      FsTreeView1.Items.AddChildFirst(node, 'Level1_' + IntToStr(i) + '_' + IntToStr(j));
  end;
end;

procedure TForm1.PaintButtons(PaintMin, PaintMax, PaintClose: Boolean);
var
  dc: HDC;
  cidx: Integer;
begin
  if not PaintMin and not PaintMax and not PaintClose then Exit;

  if csDestroying in Self.ComponentState then Exit;

  dc := GetWindowDC(Self.Handle);
  FNCCanvas.Handle := dc;

  try
    if PaintMin then
    begin
      if FMinBtnMouseDown then cidx := 2
      else if FMinBtnMouseOver then cidx := 1
      else cidx := 0;

      npdMinButton.DrawFrame(FNCCanvas.Handle, GetMinBtnRect, cidx);
    end;

    if PaintMax then
    begin
      if FMaxBtnMouseDown then cidx := 2
      else if FMaxBtnMouseOver then cidx := 1
      else cidx := 0;
      
      npdMaxButton.DrawFrame(FNCCanvas.Handle, GetMaxBtnRect, cidx);
    end;

    if PaintClose then
    begin
      if FCloseBtnMouseDown then cidx := 2
      else if FCloseBtnMouseOver then cidx := 1
      else cidx := 0;
      
      npdClose.DrawFrame(FNCCanvas.Handle, GetCloseBtnRect, cidx);
    end;
    
  finally
    FNCCanvas.Handle := 0;
    ReleaseDC(Self.Handle, dc);
  end;
end;

procedure TForm1.PaintNC;
var
  dc: HDC;
  _icon: TIcon;
  r: TRect;
begin
  if csDestroying in Self.ComponentState then Exit;
  
  dc := GetWindowDC(Self.Handle);
  FNCCanvas.Handle := dc;

  try
    npdTopBorder.DrawFrame(FNCCanvas.Handle, Rect(0, 0, Self.Width, npdTopBorder.Height), 0);
    npdBottomBorder.DrawFrame(FNCCanvas.Handle, Rect(0, Self.Height - npdBottomBorder.Height, Self.Width, Self.Height), 0);
    npdLeftBorder.DrawFrame(FNCCanvas.Handle, Rect(0, 0, npdLeftBorder.Width, Self.Height), 0);
    npdRightBorder.DrawFrame(FNCCanvas.Handle, Rect(Self.Width - npdRightBorder.Width, 0, Self.Width, Self.Height), 0);

    if Assigned(Self.Icon) and not Self.Icon.Empty then _icon := Self.Icon
    else _icon := Application.Icon;

    Windows.DrawIconEx(dc, LEFT_BORDER_WIDTH, (TITLE_BAR_HEIGHT - GetSystemMetrics(SM_CYSMICON)) div 2, _icon.Handle,
      GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON), 0, 0, DI_NORMAL or DI_COMPAT);

    Windows.SetBkMode(dc, Windows.TRANSPARENT);

    Windows.SelectObject(dc, Self.Font.Handle);
    
    Windows.SetTextColor(dc, clWhite);

    r.Left := LEFT_BORDER_WIDTH + GetSystemMetrics(SM_CXSMICON) + 4;
    r.Top := 0;
    r.Right := GetMinBtnRect.Left;
    r.Bottom := TITLE_BAR_HEIGHT;

    Windows.DrawText(dc, PChar(Self.Caption), Length(Self.Caption), r, DT_SINGLELINE or DT_CENTER or DT_VCENTER or DT_END_ELLIPSIS);

    npdMinButton.DrawFrame(FNCCanvas.Handle, GetMinBtnRect, 0);
    npdMaxButton.DrawFrame(FNCCanvas.Handle, GetMaxBtnRect, 0);
    npdClose.DrawFrame(FNCCanvas.Handle, GetCloseBtnRect, 0);

  finally
    FNCCanvas.Handle := 0;
    ReleaseDC(Self.Handle, dc);
  end;
end;

procedure TForm1.WMLButtonUp(var msgr: TWMLButtonUp);
var
  pt: TPoint;
  ht, pxy: Integer;
begin
  pt := Self.ClientToScreen(Point(msgr.XPos, msgr.YPos));
  pxy := MakeLong(pt.X, pt.Y);
  ht := Self.Perform(WM_NCHITTEST, 0, pxy);

  if ht > 1 then
  begin
    ReleaseCapture;
    Self.Perform(WM_NCLBUTTONUP, ht, pxy);
    msgr.Result := 0;
  end;

  inherited;
end;

procedure TForm1.WMMouseMove(var msgr: TWMMouseMove);
var
  pt: TPoint;
  ht, pxy: Integer;
begin
  pt := Self.ClientToScreen(Point(msgr.XPos, msgr.YPos));
  pxy := MakeLong(pt.X, pt.Y);
  ht := Self.Perform(WM_NCHITTEST, 0, pxy);

  if ht > 1 then
  begin
    Self.Perform(WM_NCMOUSEMOVE, ht, pxy);
    msgr.Result := 0;
  end;

  inherited;
end;

procedure TForm1.WMNCActivate(var msgr: TWMNCActivate);
begin
  PaintNC;
  msgr.Result := 1;
end;

procedure TForm1.WMNCCalcSize(var msgr: TWMNCCalcSize);
begin 
  msgr.CalcSize_Params.rgrc[0].Left := msgr.CalcSize_Params.rgrc[0].Left + LEFT_BORDER_WIDTH;
  msgr.CalcSize_Params.rgrc[0].Top := msgr.CalcSize_Params.rgrc[0].Top + TITLE_BAR_HEIGHT;
  msgr.CalcSize_Params.rgrc[0].Right := msgr.CalcSize_Params.rgrc[0].Right - RIGHT_BORDER_WIDTH;
  msgr.CalcSize_Params.rgrc[0].Bottom := msgr.CalcSize_Params.rgrc[0].Bottom - BOTTOM_BORDER_WIDTH;

  msgr.Result := 0;
end;

procedure TForm1.WMNCHitTest(var msgr: TWMNCHitTest);
var
  pt: TPoint;  
begin
  pt.X := msgr.XPos - Self.Left;
  pt.Y := msgr.YPos - Self.Top;

  if PtInRect(GetMinBtnRect, pt) then msgr.Result := HTMINBUTTON
  else if PtInRect(GetMaxBtnRect, pt) then msgr.Result := HTMAXBUTTON
  else if PtInRect(GetCloseBtnRect, pt) then msgr.Result := HTCLOSE
  else begin
    inherited;
  end;
end;

procedure TForm1.WMNCLButtonDown(var msgr: TWMNCLButtonDown);
var
  f1, f2, f3, f4, f5, f6: Boolean;
begin
  f1 := FMinBtnMouseDown;
  f2 := FMinBtnMouseOver;
  f3 := FMaxBtnMouseDown;
  f4 := FMaxBtnMouseOver;
  f5 := FCloseBtnMouseDown;
  f6 := FCloseBtnMouseOver;

  FMinBtnMouseOver := msgr.HitTest = HTMINBUTTON;
  FMinBtnMouseDown := FMinBtnMouseOver;
  FMaxBtnMouseOver := msgr.HitTest = HTMAXBUTTON;
  FMaxBtnMouseDown := FMaxBtnMouseOver;
  FCloseBtnMouseOver := msgr.HitTest = HTCLOSE;
  FCloseBtnMouseDown := FCloseBtnMouseOver;

  PaintButtons((f1 <> FMinBtnMouseDown) or (f2 <> FMinBtnMouseOver),
    (f3 <> FMaxBtnMouseDown) or (f4 <> FMaxBtnMouseOver),
    (f5 <> FCloseBtnMouseDown) or (f6 <> FCloseBtnMouseOver) ) ;

  if msgr.HitTest in [HTMINBUTTON, HTMAXBUTTON, HTCLOSE] then
  begin
    FCaptureMouseArea := msgr.HitTest;
    SetCapture(Self.Handle);
  end
  else inherited;
end;

procedure TForm1.WMNCLButtonUp(var msgr: TWMNCLButtonUp);
begin
  case msgr.HitTest of
    HTMINBUTTON: if FCaptureMouseArea = HTMINBUTTON then Self.Perform(WM_SYSCOMMAND, SC_MINIMIZE, 0);
    HTMAXBUTTON: if FCaptureMouseArea = HTMAXBUTTON then Self.Perform(WM_SYSCOMMAND, SC_MAXIMIZE, 0);
    HTCLOSE: if FCaptureMouseArea = HTCLOSE then Self.Perform(WM_SYSCOMMAND, SC_CLOSE, 0);
    else inherited;
  end;

  ReleaseCapture;
end;

procedure TForm1.WMNCMouseLeave(var msgr: TWMNCMouseMove);
var
  f1, f2, f3, f4, f5, f6: Boolean;
  pt: DWORD;
  ht: Integer;
begin
  inherited;

  pt := GetMessagePos;

  ht := Self.Perform(WM_NCHITTEST, 0, pt);
  
  if ht <= 1 then
  begin
    f1 := FMinBtnMouseDown;
    f2 := FMinBtnMouseOver;
    f3 := FMaxBtnMouseDown;
    f4 := FMaxBtnMouseOver;
    f5 := FCloseBtnMouseDown;
    f6 := FCloseBtnMouseOver;

    FMinBtnMouseOver := False;
    FMinBtnMouseDown := False;
    FMaxBtnMouseOver := False;
    FMaxBtnMouseDown := False;
    FCloseBtnMouseOver := False;
    FCloseBtnMouseDown := False;

    PaintButtons((f1 <> FMinBtnMouseDown) or (f2 <> FMinBtnMouseOver),
      (f3 <> FMaxBtnMouseDown) or (f4 <> FMaxBtnMouseOver),
      (f5 <> FCloseBtnMouseDown) or (f6 <> FCloseBtnMouseOver) );
  end;
end;

procedure TForm1.WMNCMouseMove(var msgr: TWMNCMouseMove);
var
  f1, f2, f3, f4, f5, f6: Boolean;
begin
  inherited;

  f1 := FMinBtnMouseDown;
  f2 := FMinBtnMouseOver;
  f3 := FMaxBtnMouseDown;
  f4 := FMaxBtnMouseOver;
  f5 := FCloseBtnMouseDown;
  f6 := FCloseBtnMouseOver;

  FMinBtnMouseOver := msgr.HitTest = HTMINBUTTON;
  FMinBtnMouseDown := FMinBtnMouseOver and FMinBtnMouseDown;
  FMaxBtnMouseOver := msgr.HitTest = HTMAXBUTTON;
  FMaxBtnMouseDown := FMaxBtnMouseOver and FMaxBtnMouseDown;
  FCloseBtnMouseOver := msgr.HitTest = HTCLOSE;
  FCloseBtnMouseDown := FCloseBtnMouseOver and FCloseBtnMouseDown;

  PaintButtons((f1 <> FMinBtnMouseDown) or (f2 <> FMinBtnMouseOver),
    (f3 <> FMaxBtnMouseDown) or (f4 <> FMaxBtnMouseOver),
    (f5 <> FCloseBtnMouseDown) or (f6 <> FCloseBtnMouseOver) ) ;
end;

procedure TForm1.WMNCPaint(var msgr: TWMNCPaint);
begin
  Self.PaintNC;
end;

end.
