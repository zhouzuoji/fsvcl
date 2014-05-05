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

end.
