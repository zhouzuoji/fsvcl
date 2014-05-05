unit FSSkinCtrls;

interface

uses
  SysUtils, Classes, Consts, Windows, Graphics, Controls, Messages, StdCtrls, ExtCtrls, ComCtrls,
  FSVclBase, FSGraphics, FSControls, FSScrollControls, Themes, FSStdCtrls;

type
  TFsSkinButton = class(TFsCustomButton)
  private
    FPicture: TFsDrawable;
    FDisablePicture: TFsDrawable;
    FMouseOverPicture: TFsDrawable;
    FMouseDownPicture: TFsDrawable;
    procedure SetLinkDrawable(var field: TFsDrawable; value: TFsDrawable);
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(const Value: TFsDrawable);
    procedure SetMouseDownPicture(const Value: TFsDrawable);
    procedure SetMouseOverPicture(const Value: TFsDrawable);
    procedure SetDisablePicture(const Value: TFsDrawable);
    function GetDrawable: TFsDrawable;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetPictureSize(out width: Integer): Integer; override;
    procedure DrawPicture(const Rect: TRect); override;
  public
    destructor Destroy; override;
  published
    property AllowDown;
    property Background;
    property Down;
    property Group;
    property Picture: TFsDrawable read FPicture write SetPicture;
    property DisablePicture: TFsDrawable read FDisablePicture write SetDisablePicture;
    property MouseOverPicture: TFsDrawable read FMouseOverPicture write SetMouseOverPicture;
    property MouseDownPicture: TFsDrawable read FMouseDownPicture write SetMouseDownPicture;
  end;

  TFsSkinCheckBox = class(TFsCustomCheckBox)
  private
    FCheckedPicture: TFsDrawable;
    FUnCheckedPicture: TFsDrawable;
    procedure SetCheckedPicture(const Value: TFsDrawable);
    procedure SetUnCheckedPicture(const Value: TFsDrawable);
    procedure PictureChanged(Sender: TObject);
  protected
    function GetDrawable: TFsDrawable;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure GetImageSize(out w, h: Integer); override;
    procedure DrawMark(const Rect: TRect); override;
  public
    destructor Destroy; override;
  published
    property CheckedPicture: TFsDrawable read FCheckedPicture write SetCheckedPicture;
    property UnCheckedPicture: TFsDrawable read FUnCheckedPicture write SetUnCheckedPicture;
  end;

  TFsSkinCombobox = class(TFsCustomCombobox)
  private
    FPicture: TFsDrawable;
    FDroppedDownPicture: TFsDrawable;
    procedure PictureChanged(Sender: TObject);
    procedure SetPicture(const Value: TFsDrawable);
    procedure SetDroppedDownPicture(const Value: TFsDrawable);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawButton(const Rect: TRect); override;
  public
    destructor Destroy; override;
  published
    property Align;
    property AutoComplete default True;
    property AutoCompleteDelay default 500;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
    property ButtonWidth;
    property BorderColor;
    property BorderColorHover;
    property ShowTip;
    property Tip;
    property TipFont;
    property TextFont;
    property Picture: TFsDrawable read FPicture write SetPicture;
    property DroppedDownPicture: TFsDrawable read FDroppedDownPicture write SetDroppedDownPicture;
  end;

implementation

{ TFsSkinButton }

destructor TFsSkinButton.Destroy;
begin
  SetPicture(nil);
  SetMouseDownPicture(nil);
  SetMouseOverPicture(nil);
  SetDisablePicture(nil);
  inherited;
end;

procedure TFsSkinButton.DrawPicture(const Rect: TRect);
var
  drawable: TFsDrawable;
begin
  drawable := Self.GetDrawable;

  if Assigned(drawable) then
    drawable.Draw(Self.Canvas, Rect);
end;

function TFsSkinButton.GetDrawable: TFsDrawable;
begin
  if not Self.Enabled then Result := FDisablePicture
  else if AllowDown and Down then Result := FMouseDownPicture
  else if mfLButtonDown in FMouseFlag then Result := FMouseDownPicture
  else if mfMouseOver in FMouseFlag then Result := FMouseOverPicture
  else Result := FPicture;

  if not Assigned(Result) or Result.Empty then
    Result := FPicture;
end;

function TFsSkinButton.GetPictureSize(out width: Integer): Integer;
var
  drawable: TFsDrawable;
begin
  drawable := Self.GetDrawable;

  if Assigned(drawable) and not drawable.Empty then
  begin
    width := drawable.Width;
    Result := drawable.Height;
  end
  else begin
    width := 0;
    Result := 0;
  end;
end;

procedure TFsSkinButton.Notification(AComponent: TComponent; Operation: TOperation);
var
  changed: Boolean;
begin
  inherited;

  changed := False;

  if AComponent = FPicture then
  begin
    FPicture := nil;
    changed := True;
  end;

  if AComponent = FDisablePicture then
  begin
    FDisablePicture := nil;
    changed := True;
  end;

  if AComponent = FMouseOverPicture then
  begin
    FMouseOverPicture := nil;
    changed := True;
  end;

  if AComponent = FMouseDownPicture then
  begin
    FMouseDownPicture := nil;
    changed := True;
  end;

  if changed then Self.AutoSizeAndInvalidate;
end;

procedure TFsSkinButton.PictureChanged(Sender: TObject);
begin
  if Sender = GetDrawable then Self.AutoSizeAndInvalidate;
end;

procedure TFsSkinButton.SetDisablePicture(const Value: TFsDrawable);
begin
  SetLinkDrawable(FDisablePicture, Value);
end;

procedure TFsSkinButton.SetLinkDrawable(var field: TFsDrawable; value: TFsDrawable);
begin
  if field <> Value then
  begin
    if Assigned(field) then
    begin
      field.RemoveOnChangeListener(Self.PictureChanged);
      field.RemoveFreeNotification(Self);
    end;

    field := Value;

    if Assigned(field) then
    begin
      field.AddOnChangeListener(Self.PictureChanged);
      field.FreeNotification(Self);
    end;

    Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsSkinButton.SetMouseDownPicture(const Value: TFsDrawable);
begin
  SetLinkDrawable(FMouseDownPicture, Value);
end;

procedure TFsSkinButton.SetMouseOverPicture(const Value: TFsDrawable);
begin
  SetLinkDrawable(FMouseOverPicture, Value);
end;

procedure TFsSkinButton.SetPicture(const Value: TFsDrawable);
begin
  SetLinkDrawable(FPicture, Value);
end;

{ TFsSkinCheckBox }

procedure TFsSkinCheckBox.PictureChanged(Sender: TObject);
begin
  if ( Checked and (Sender = FCheckedPicture) ) or ( not Checked and (Sender = FUnCheckedPicture) ) then
    Self.AutoSizeAndInvalidate;
end;

destructor TFsSkinCheckBox.Destroy;
begin
  SetCheckedPicture(nil);
  SetUnCheckedPicture(nil);
  inherited;
end;

procedure TFsSkinCheckBox.DrawMark(const Rect: TRect);
var
  drawable: TFsDrawable;
begin
  inherited;

  drawable := Self.GetDrawable;

  if Assigned(drawable) then
    drawable.Draw(Canvas, Rect);
end;

function TFsSkinCheckBox.GetDrawable: TFsDrawable;
begin
  if Checked then Result := FCheckedPicture
  else Result := FUnCheckedPicture;
end;

procedure TFsSkinCheckBox.GetImageSize(out w, h: Integer);
var
  drawable: TFsDrawable;
begin
  inherited;

  drawable := GetDrawable;

  if Assigned(drawable) then 
  begin
    w := drawable.Width;
    h := drawable.Height;
  end
  else begin
    w := 0;
    h := 0;
  end;
end;

procedure TFsSkinCheckBox.Notification(AComponent: TComponent; Operation: TOperation);
var
  changed: Boolean;
begin
  inherited;

  changed := False;

  if AComponent = FCheckedPicture then
  begin
    FCheckedPicture := nil;

    if Checked then changed := True;
  end;

  if AComponent = FUnCheckedPicture then
  begin
    FUnCheckedPicture := nil;
    if not Checked then changed := True;
  end;

  if changed then Self.AutoSizeAndInvalidate;
end;

procedure TFsSkinCheckBox.SetCheckedPicture(const Value: TFsDrawable);
begin
  if FCheckedPicture <> Value then
  begin
    if Assigned(FCheckedPicture) then
    begin
      FCheckedPicture.RemoveOnChangeListener(Self.PictureChanged);
      FCheckedPicture.RemoveFreeNotification(Self);
    end;

    FCheckedPicture := Value;

    if Assigned(FCheckedPicture) then
    begin
      FCheckedPicture.AddOnChangeListener(Self.PictureChanged);
      FCheckedPicture.FreeNotification(Self);
    end;
      
    Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsSkinCheckBox.SetUnCheckedPicture(const Value: TFsDrawable);
begin
  if FUnCheckedPicture <> Value then
  begin
    if Assigned(FUnCheckedPicture) then
    begin
      FUnCheckedPicture.RemoveOnChangeListener(Self.PictureChanged);
      FUnCheckedPicture.RemoveFreeNotification(Self);
    end;

    FUnCheckedPicture := Value;

    if Assigned(FUnCheckedPicture) then
    begin
      FUnCheckedPicture.AddOnChangeListener(Self.PictureChanged);
      FUnCheckedPicture.FreeNotification(Self);
    end;
      
    Self.AutoSizeAndInvalidate;
  end;
end;

{ TFsSkinCombobox }

procedure TFsSkinCombobox.PictureChanged(Sender: TObject);
begin
  if ( not Self.DroppedDown and (Sender = FPicture) ) or ( Self.DroppedDown and (Sender = FDroppedDownPicture) ) then
    Self.Invalidate;    
end;

destructor TFsSkinCombobox.Destroy;
begin
  SetPicture(nil);
  SetDroppedDownPicture(nil);
  inherited;
end;

procedure TFsSkinCombobox.DrawButton(const Rect: TRect);
var
  drawable: TFsDrawable;
begin
  inherited;

  if Self.DroppedDown then drawable := FDroppedDownPicture
  else drawable := FPicture;

  if not Assigned(drawable) then drawable := FPicture;

  if Assigned(drawable) then drawable.DrawCenter(Canvas, Rect);
end;

procedure TFsSkinCombobox.Notification(AComponent: TComponent; Operation: TOperation);
var
  changed: Boolean;
begin
  inherited;

  changed := False;

  if AComponent = FPicture then
  begin
    FPicture := nil;

    if not DroppedDown then changed := True;
  end;

  if AComponent = FDroppedDownPicture then
  begin
    FDroppedDownPicture := nil;
    if DroppedDown then changed := True;
  end;

  if changed then Self.Invalidate;
end;

procedure TFsSkinCombobox.SetPicture(const Value: TFsDrawable);
begin
  if FPicture <> Value then
  begin
    if Assigned(FPicture) then
    begin
      FPicture.RemoveOnChangeListener(Self.PictureChanged);
      FPicture.RemoveFreeNotification(Self);
    end;

    FPicture := Value;

    if Assigned(FPicture) then
    begin
      FPicture.AddOnChangeListener(Self.PictureChanged);
      FPicture.FreeNotification(Self);
    end;

    Self.Invalidate;
  end;
end;

procedure TFsSkinCombobox.SetDroppedDownPicture(const Value: TFsDrawable);
begin
  if FDroppedDownPicture <> Value then
  begin
    if Assigned(FDroppedDownPicture) then
    begin
      FDroppedDownPicture.RemoveOnChangeListener(Self.PictureChanged);
      FDroppedDownPicture.RemoveFreeNotification(Self);
    end;

    FDroppedDownPicture := Value;

    if Assigned(FDroppedDownPicture) then
    begin
      FDroppedDownPicture.AddOnChangeListener(Self.PictureChanged);
      FDroppedDownPicture.FreeNotification(Self);
    end;

    Self.Invalidate;
  end;
end;

end.
