unit FSEdit;

interface

uses
  SysUtils, Classes, Consts, Windows, Graphics, Controls, Messages, StdCtrls, ExtCtrls, ComCtrls,
  Forms, Dialogs, FSVclBase, FSGraphics, FSControls, FSScrollControls, Themes;

type
  TFsCustomEdit = class(TCustomEdit)
  private
    FBorderColorHover: TColor;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FNumberOnly: Boolean;
    FTip: string;
    FTipFont: TFont;
    FTextFont: TFont;
    FShowingTip: Boolean;
    FShowTip: Boolean;
    FOriginPasswordChar: Char; 
    procedure TipFontChanged(Sender: TObject);
    procedure TextFontChanged(Sender: TObject);
    procedure SetTip(const Value: string);
    procedure SetTipFont(const Value: TFont);
    procedure SetTextFont(const Value: TFont);
    procedure SetShowingTip(const Value: Boolean);
    procedure SetBorderColorHover(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetNumberOnly(const Value: Boolean);
    procedure SetShowTip(const Value: Boolean);
  protected
    FMouseIn: Boolean;
    procedure NCChanged;
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CMMouseEnter(var msgr: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msgr: TMessage); message CM_MOUSELEAVE;
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPAINT(var msgr: TWMNCPaint); message WM_NCPAINT;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderColorHover: TColor read FBorderColorHover write SetBorderColorHover;
    property NumberOnly: Boolean read FNumberOnly write SetNumberOnly;
    property ShowTip: Boolean read FShowTip write SetShowTip;
    property Tip: string read FTip write SetTip;
    property TipFont: TFont read FTipFont write SetTipFont;
    property TextFont: TFont read FTextFont write SetTextFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShowingTip: Boolean read FShowingTip write SetShowingTip;
  end;

  TFsEdit = class(TFsCustomEdit)
  published
    property ShowTip;
    property Tip;
    property TipFont;
    property TextFont;
    property BorderColor;
    property BorderColorHover;
    property NumberOnly;
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TFsCustomButtonEdit = class(TFsCustomEdit)
  private
    FButtonPicture: TPicture;
    FSpace: Integer;
    procedure SetButtonPicture(const Value: TPicture);
    procedure SetSpace(const Value: Integer);
  protected
    FNCCanvas: TCanvas;
    procedure WMNCPAINT(var msgr: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCHitTest(var msgr: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var msgr: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCCalcSize(var msgr: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure DoClickButton; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ButtonPicture: TPicture read FButtonPicture write SetButtonPicture;
    property Space: Integer read FSpace write SetSpace;
  end;

  TFsButtonEdit = class(TFsCustomButtonEdit)
  private
    FOnClickButton: TNotifyEvent;
  protected
    procedure DoClickButton; override;
  published
    property ButtonPicture;
    property Space;
    property OnClickButton: TNotifyEvent read FOnClickButton write FOnClickButton;
    property ShowTip;
    property Tip;
    property TipFont;
    property TextFont;
    property BorderColor;
    property BorderColorHover;
    property NumberOnly;
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TFsCustomEdit }

procedure TFsCustomEdit.CMMouseEnter(var msgr: TMessage);
begin
  FMouseIn := True;
  inherited;
end;

procedure TFsCustomEdit.CMMouseLeave(var msgr: TMessage);
begin
  FMouseIn := False;
  inherited;
end;

constructor TFsCustomEdit.Create(AOwner: TComponent);
begin
  inherited;
  Self.ParentCtl3D := False;
  Self.Ctl3D := True;
  FBorderWidth := 1;
  FBorderColorHover := RGB(123, 228, 255);
  FBorderColor := RGB(78, 160, 209);
  FTipFont := TFont.Create;
  FTipFont.Color := clGrayText;
  FTipFont.OnChange := Self.TipFontChanged;
  FTextFont := TFont.Create;
  FTextFont.OnChange := Self.TextFontChanged;
  Font.Assign(FTextFont);
end;

procedure TFsCustomEdit.DoEnter;
begin
  ShowingTip := False;
  inherited;
end;

procedure TFsCustomEdit.DoExit;
begin
  if Text = '' then ShowingTip := True;
  inherited;
end;

procedure TFsCustomEdit.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    SetShowingTip(Text = '');
  end;
end;

procedure TFsCustomEdit.SetShowingTip(const Value: Boolean);
begin
  if FShowTip then
  begin
    if FShowingTip <> Value then
    begin
      FShowingTip := Value;

      if ShowingTip then
      begin
        FOriginPasswordChar := PasswordChar;
        PasswordChar := #0;
        Text := FTip;
        Font.Assign(FTipFont);
      end
      else begin
        PasswordChar := FOriginPasswordChar;
        Text := '';
        Font.Assign(FTextFont);
      end;
    end;
  end;
end;

procedure TFsCustomEdit.SetShowTip(const Value: Boolean);
begin
  if FShowTip <> Value then
  begin
    FShowTip := Value;

    if not (csLoading in ComponentState) then
    begin
      if ShowTip then
      begin
        if Text = '' then  SetShowingTip(True);
      end
      else SetShowingTip(False);
    end;
  end;
end;

procedure TFsCustomEdit.SetTextFont(const Value: TFont);
begin
  FTextFont.Assign(Value);
end;

procedure TFsCustomEdit.SetTip(const Value: string);
begin
  FTip := Value;
  if FShowingTip then Self.Text := FTip;
end;

procedure TFsCustomEdit.SetTipFont(const Value: TFont);
begin
  FTipFont.Assign(Value);
end;

procedure TFsCustomEdit.TextFontChanged(Sender: TObject);
begin
  if not FShowingTip then Self.Font.Assign(FTextFont);
end;

procedure TFsCustomEdit.TipFontChanged(Sender: TObject);
begin
  if FShowingTip then Self.Font.Assign(FTipFont);
end;

procedure TFsCustomEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;

  if FNumberOnly then Params.Style := Params.Style or ES_NUMBER
  else Params.Style := Params.Style and not ES_NUMBER;
end;

destructor TFsCustomEdit.Destroy;
begin
  FTipFont.Free;
  FTextFont.Free;
  inherited;
end;

procedure TFsCustomEdit.NCChanged;
begin
  if HandleAllocated then
  begin
    SetWindowPos(Handle, 0, 0,0,0,0, SWP_NOACTIVATE or
    SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED);

    if Visible then Self.Invalidate;
  end;
end;

procedure TFsCustomEdit.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Self.Invalidate;
  end;
end;

procedure TFsCustomEdit.SetBorderColorHover(const Value: TColor);
begin
  if FBorderColorHover <> Value then
  begin
    FBorderColorHover := Value;
    Self.Invalidate;
  end;
end;

procedure TFsCustomEdit.SetBorderWidth(const Value: Integer);
begin
  if (FBorderWidth <> Value) and (Value >= 0) then
  begin
    FBorderWidth := Value;
    Self.NCChanged;
  end;
end;

procedure TFsCustomEdit.SetNumberOnly(const Value: Boolean);
var
  style: DWORD;
begin
  FNumberOnly := Value;

  if HandleAllocated then
  begin
    style := GetWindowLong(WindowHandle, GWL_STYLE);

    if FNumberOnly then style := style or ES_NUMBER
    else style := style and not ES_NUMBER;

    SetWindowLong(WindowHandle, GWL_STYLE, style);
  end;
end;

procedure TFsCustomEdit.WMNCCalcSize(var msgr: TWMNCCalcSize);
var
  pr: PRect;
begin
  pr := @msgr.CalcSize_Params.rgrc[0];
  Inc(pr.Left, FBorderWidth);
  Inc(pr.Top, FBorderWidth);
  Dec(pr.Right, FBorderWidth);
  Dec(pr.Bottom, FBorderWidth);
  msgr.Result := 0;
end;

procedure TFsCustomEdit.WMNCPAINT(var msgr: TWMNCPaint);
var
  dc: HDC;
  hb: HBRUSH;
  bgc: DWORD;
begin
  dc := GetWindowDC(Handle);

  try    
    if FMouseIn then bgc := ColorToRGB(FBorderColorHover)
    else bgc := ColorToRGB(FBorderColor);
    
    hb := CreateSolidBrush(bgc);
    Windows.FrameRect(dc, Rect(0, 0, Self.Width, Self.Height), hb);
    DeleteObject(hb);
    
    msgr.Result := 0;
  finally
    ReleaseDC(Handle, dc);
  end;
end;

{ TFsCustomButtonEdit }

constructor TFsCustomButtonEdit.Create(AOwner: TComponent);
begin
  inherited;
  FButtonPicture := TPicture.Create;
  FNCCanvas := TCanvas.Create;
end;

destructor TFsCustomButtonEdit.Destroy;
begin
  FButtonPicture.Free;
  FNCCanvas.Free;
  inherited;
end;

procedure TFsCustomButtonEdit.SetSpace(const Value: Integer);
begin
  if (FSpace <> Value) and (Value >= 0) then
  begin
    FSpace := Value;
    Self.NCChanged;
  end;
end;

procedure TFsCustomButtonEdit.WMNCCalcSize(var msgr: TWMNCCalcSize);
var
  pr: PRect;
begin
  inherited;

  pr := @msgr.CalcSize_Params.rgrc[0];

  if Assigned(FButtonPicture.Graphic) and not FButtonPicture.Graphic.Empty then
    Dec(pr.Right, FButtonPicture.Width + FSpace);

  msgr.Result := 0;
end;

procedure TFsCustomButtonEdit.WMNCHitTest(var msgr: TWMNCHitTest);
var
  pt: TPoint;
  r, rc: TRect;
begin
  if FButtonPicture.Width > 0 then
  begin
    pt.X := msgr.XPos;
    pt.Y := msgr.YPos;
    Windows.ScreenToClient(Handle, pt);
    Windows.GetClientRect(Handle, rc);

    r.Left := rc.Right;
    r.Right := r.Left + Self.Space + FButtonPicture.Width;
    r.Top := rc.Top;
    r.Bottom := rc.Bottom;

    if PtInRect(r, pt) then msgr.Result := HTOBJECT
    else inherited;
  end
  else inherited;
end;

procedure TFsCustomButtonEdit.WMNCLButtonDown(var msgr: TWMNCLButtonDown);
begin
  if msgr.HitTest = HTOBJECT then
  begin
    Self.DoClickButton;
    msgr.Result := 0;
  end
  else inherited;
end;

procedure TFsCustomButtonEdit.WMNCPAINT(var msgr: TWMNCPaint);
var
  dc: HDC;
  r: TRect;
begin
  inherited;

  if Assigned(FButtonPicture.Graphic) and not FButtonPicture.Graphic.Empty then
  begin
    r.Left := Self.Width - BorderWidth - FSpace - FButtonPicture.Width;
    r.Right := Self.Width - BorderWidth;

    r.Top := (Self.Height - FButtonPicture.Height) div 2;
    r.Bottom := Self.Height - r.Top;

    dc := GetWindowDC(Self.Handle);

    FNCCanvas.Handle := dc;

    try
      FNCCanvas.Brush.Color := Self.Color;
      FNCCanvas.FillRect(Rect(r.Left, BorderWidth, r.Right, Self.Height - BorderWidth));

      FNCCanvas.Draw(r.Left + FSpace shr 1, r.Top, FButtonPicture.Graphic);

      msgr.Result := 0;
    finally
      FNCCanvas.Handle := 0;
      ReleaseDC(Self.Handle, dc);
    end;
  end;
end;

procedure TFsCustomButtonEdit.SetButtonPicture(const Value: TPicture);
begin
  FButtonPicture.Assign(Value);
  Self.NCChanged;
end;

{ TFsButtonEdit }

procedure TFsButtonEdit.DoClickButton;
begin
  if Assigned(FOnClickButton) then FOnClickButton(Self);
end;

end.
