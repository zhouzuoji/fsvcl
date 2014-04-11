unit FSTheme;

interface

uses
  SysUtils, Classes, Windows, Graphics, FsGraphics;

type
  TFsButtonStyle = class(TComponent)
  private
    FNormal: TFsNinePitchDrawable;
    FDisable: TFsNinePitchDrawable;
    FMouseOver: TFsNinePitchDrawable;
    FMouseDown: TFsNinePitchDrawable;
    procedure SetDisable(const Value: TFsNinePitchDrawable);
    procedure SetMouseDown(const Value: TFsNinePitchDrawable);
    procedure SetMouseOver(const Value: TFsNinePitchDrawable);
    procedure SetNormal(const Value: TFsNinePitchDrawable);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Normal: TFsNinePitchDrawable read FNormal write SetNormal;
    property Disable: TFsNinePitchDrawable read FDisable write SetDisable;
    property MouseOver: TFsNinePitchDrawable read FMouseOver write SetMouseOver;
    property MouseDown: TFsNinePitchDrawable read FMouseDown write SetMouseDown;
  end;

  TFsTheme = class(TComponent)
  private
    FButtons: TFsButtonStyle;
    procedure SetButtons(const Value: TFsButtonStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Buttons: TFsButtonStyle read FButtons write SetButtons;
  end;

var
  FSDefaultTheme: TFsTheme;

implementation

{ TFsTheme }

constructor TFsTheme.Create(AOwner: TComponent);
begin
  inherited;

  FButtons := TFsButtonStyle.Create(Self);

  if not Assigned(FSDefaultTheme) then
    FSDefaultTheme := Self;
end;

destructor TFsTheme.Destroy;
begin
  if FSDefaultTheme = Self then FSDefaultTheme := nil;
  
  inherited;
end;

procedure TFsTheme.SetButtons(const Value: TFsButtonStyle);
begin
  FButtons.Assign(Value);
end;

{ TFsButtonStyle }

constructor TFsButtonStyle.Create(AOwner: TComponent);
begin
  inherited;
  FNormal := TFsNinePitchDrawable.Create(Self);
  FDisable := TFsNinePitchDrawable.Create(Self);
  FMouseOver := TFsNinePitchDrawable.Create(Self);
  FMouseDown := TFsNinePitchDrawable.Create(Self);
end;

destructor TFsButtonStyle.Destroy;
begin

  inherited;
end;

procedure TFsButtonStyle.SetDisable(const Value: TFsNinePitchDrawable);
begin
  FDisable.Assign(Value);
end;

procedure TFsButtonStyle.SetMouseDown(const Value: TFsNinePitchDrawable);
begin
  FMouseDown.Assign(Value);
end;

procedure TFsButtonStyle.SetMouseOver(const Value: TFsNinePitchDrawable);
begin
  FMouseOver.Assign(Value);
end;

procedure TFsButtonStyle.SetNormal(const Value: TFsNinePitchDrawable);
begin
  FNormal.Assign(Value);
end;

end.
