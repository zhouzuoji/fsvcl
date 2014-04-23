unit FSSkinCtrls;

interface

uses
  SysUtils, Classes, Consts, Windows, Graphics, Controls, Messages, StdCtrls, ExtCtrls, ComCtrls,
  FSVclBase, FSGraphics, FsControls, FSScrollControls, Themes, FSStdCtrls;

type
  TFsSkinButton = class(TFsCustomButton)
  private
    FPicture: TFsDrawable;
    FDisablePicture: TFsDrawable;
    FMouseOverPicture: TFsDrawable;
    FMouseDownPicture: TFsDrawable;
    procedure PictureChanged(Sender: TObject; ID: TNotifyID);
    procedure SetPicture(const Value: TFsDrawable);
    procedure SetMouseDownPicture(const Value: TFsDrawable);
    procedure SetMouseOverPicture(const Value: TFsDrawable);
    procedure SetDisablePicture(const Value: TFsDrawable);
    function GetDrawable: TFsDrawable;
  protected
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
    procedure CheckedPictureChanged(Sender: TObject; ID: TNotifyID);
    procedure UnCheckedPictureChanged(Sender: TObject; ID: TNotifyID);
  protected
    function GetDrawable: TFsDrawable;
    procedure GetImageSize(out w, h: Integer); override;
    procedure DrawMark(const Rect: TRect); override;
  public
    constructor Create(Owner: TComponent); override;
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
  begin
    if drawable is TFsSingleDrawable then
      TFsSingleDrawable(drawable).Draw(Self.Canvas, Rect)
    else if drawable is TFsMultiFrameDrawable then
      TFsMultiFrameDrawable(drawable).DrawFrame(Canvas.Handle, Rect, 0);
  end;
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

procedure TFsSkinButton.PictureChanged(Sender: TObject; ID: TNotifyID);
begin
  if ID = niDestroy then
  begin
    if Sender = FPicture then
    begin
      FPicture := nil;
      Self.AutoSizeAndInvalidate;
    end;

    if Sender = FDisablePicture then
    begin
      FDisablePicture := nil;
      Self.AutoSizeAndInvalidate;
    end;

    if Sender = FMouseOverPicture then
    begin
      FMouseOverPicture := nil;
      Self.AutoSizeAndInvalidate;
    end;

    if Sender = FMouseDownPicture then
    begin
      FMouseDownPicture := nil;
      Self.AutoSizeAndInvalidate;
    end;
  end
  else if ID = niChange then
  begin
    if Sender = GetDrawable then Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsSkinButton.SetDisablePicture(const Value: TFsDrawable);
begin
  if FDisablePicture <> Value then
  begin
    if Assigned(FDisablePicture) then
      FDisablePicture.RemoveOnChangeListener(Self.PictureChanged);

    FDisablePicture := Value;

    if Assigned(FDisablePicture) then
      FDisablePicture.AddOnChangeListener(Self.PictureChanged);
      
    Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsSkinButton.SetMouseDownPicture(const Value: TFsDrawable);
begin
  if FMouseDownPicture <> Value then
  begin
    if Assigned(FMouseDownPicture) then
      FMouseDownPicture.RemoveOnChangeListener(Self.PictureChanged);

    FMouseDownPicture := Value;

    if Assigned(FMouseDownPicture) then
      FMouseDownPicture.AddOnChangeListener(Self.PictureChanged);
      
    Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsSkinButton.SetMouseOverPicture(const Value: TFsDrawable);
begin
  if FMouseOverPicture <> Value then
  begin
    if Assigned(FMouseOverPicture) then
      FMouseOverPicture.RemoveOnChangeListener(Self.PictureChanged);

    FMouseOverPicture := Value;

    if Assigned(FMouseOverPicture) then
      FMouseOverPicture.AddOnChangeListener(Self.PictureChanged);
      
    Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsSkinButton.SetPicture(const Value: TFsDrawable);
begin
  if FPicture <> Value then
  begin
    if Assigned(FPicture) then
      FPicture.RemoveOnChangeListener(Self.PictureChanged);

    FPicture := Value;

    if Assigned(FPicture) then
      FPicture.AddOnChangeListener(Self.PictureChanged);
      
    Self.AutoSizeAndInvalidate;
  end;
end;

{ TFsSkinCheckBox }

procedure TFsSkinCheckBox.CheckedPictureChanged(Sender: TObject; ID: TNotifyID);
begin
  if ID = niDestroy then FCheckedPicture := nil;
  Self.AutoSizeAndInvalidate;
end;

constructor TFsSkinCheckBox.Create(Owner: TComponent);
begin
  inherited;

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

  if drawable is TFsSingleDrawable then
    TFsSingleDrawable(drawable).Draw(Canvas, Rect)
  else if drawable is TFsMultiFrameDrawable then
    TFsMultiFrameDrawable(drawable).DrawFrame(Canvas.Handle, Rect, 0);
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

procedure TFsSkinCheckBox.SetCheckedPicture(const Value: TFsDrawable);
begin
  if FCheckedPicture <> Value then
  begin
    if Assigned(FCheckedPicture) then
      FCheckedPicture.RemoveOnChangeListener(Self.CheckedPictureChanged);

    FCheckedPicture := Value;

    if Assigned(FCheckedPicture) then
      FCheckedPicture.AddOnChangeListener(Self.CheckedPictureChanged);
      
    Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsSkinCheckBox.SetUnCheckedPicture(const Value: TFsDrawable);
begin
  if FUnCheckedPicture <> Value then
  begin
    if Assigned(FUnCheckedPicture) then
      FUnCheckedPicture.RemoveOnChangeListener(Self.UnCheckedPictureChanged);

    FUnCheckedPicture := Value;

    if Assigned(FUnCheckedPicture) then
      FUnCheckedPicture.AddOnChangeListener(Self.UnCheckedPictureChanged);
      
    Self.AutoSizeAndInvalidate;
  end;
end;

procedure TFsSkinCheckBox.UnCheckedPictureChanged(Sender: TObject; ID: TNotifyID);
begin
  if ID = niDestroy then FUnCheckedPicture := nil;
  Self.AutoSizeAndInvalidate;
end;

end.
