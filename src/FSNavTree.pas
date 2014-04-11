unit FSNavTree;

interface

uses
  SysUtils, Classes, Windows, Messages, Graphics, Controls, FSVclBase, FSControls;

type
  TFsNavTree = class;
  TFsNavTreeNodes = class;

  TFsNavTreeCatalog = class(TCollectionItem)
  private
    FText: string;
    FNodes: TFsNavTreeNodes;
    procedure SetText(const Value: string);
    procedure SetNodes(const Value: TFsNavTreeNodes);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: string read FText write SetText;
    property Nodes: TFsNavTreeNodes read FNodes write SetNodes;
  end;

  TFsNavTreeCatalogs = class(TCollection)
  private
    FTree: TFsNavTree;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ATree: TFsNavTree);
    property Tree: TFsNavTree read FTree;
  end;

  TFsNavTreeNode = class(TCollectionItem)
  private
    FText: string;
    FChildNodes: TFsNavTreeNodes;
    FExpanded: Boolean;
    procedure SetText(const Value: string);
    procedure SetChildNodes(const Value: TFsNavTreeNodes);
    procedure SetExpanded(const Value: Boolean);
    function GetCatalog: TFsNavTreeCatalog;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: string read FText write SetText;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property ChildNodes: TFsNavTreeNodes read FChildNodes write SetChildNodes;
    property Catalog: TFsNavTreeCatalog read GetCatalog;
  end;

  TFsNavTreeNodes = class(TCollection)
  private
    FCatalog: TFsNavTreeCatalog;
    FParent: TFsNavTreeNode;
    function GetCatalog: TFsNavTreeCatalog;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ACatalog: TFsNavTreeCatalog); reintroduce; overload;
    constructor Create(AParent: TFsNavTreeNode); reintroduce; overload;
    property Catalog: TFsNavTreeCatalog read GetCatalog;
    property Parent: TFsNavTreeNode read FParent;
  end;

  TFsNavTree = class(TFsCustomControl)
  private
    FCatalogs: TFsNavTreeCatalogs;
  protected
    procedure PaintClientRect(const rc: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property Ctl3D;
    property Visible;
    property ParentBackground;
    property ParentCtl3D;
    property ScrollBarDrawer;
    property ScrollBars;
    property Catalogs: TFsNavTreeCatalogs read FCatalogs;
  end;    

implementation

{ TFsNavTree }

constructor TFsNavTree.Create(AOwner: TComponent);
begin
  inherited;
  Width := 250;
  Height := 360;
  FCatalogs := TFsNavTreeCatalogs.Create(Self);
end;

destructor TFsNavTree.Destroy;
begin
  FCatalogs.Free;
  inherited;
end;

procedure TFsNavTree.PaintClientRect(const rc: TRect);
begin
  inherited;
  {Self.Canvas.Brush.Color := clRed;
  Self.Canvas.Pen.Color := clGreen;
  Self.Canvas.Rectangle(rc);}
end;

{ TFsNavTreeNode }

procedure TFsNavTreeNode.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TFsNavTreeNode then
    Self.FText := TFsNavTreeNode(Source).Text
  else inherited;
end;

constructor TFsNavTreeNode.Create(Collection: TCollection);
begin
  inherited;
  FChildNodes := TFsNavTreeNodes.Create(Self);
  FExpanded := False;
end;

destructor TFsNavTreeNode.Destroy;
begin
  FChildNodes.Free;
  inherited;
end;

function TFsNavTreeNode.GetCatalog: TFsNavTreeCatalog;
begin
  if Assigned(Collection) then Result := TFsNavTreeNodes(Collection).Catalog
  else Result := nil;
end;

function TFsNavTreeNode.GetDisplayName: string;
begin
  Result := FText;
end;

procedure TFsNavTreeNode.SetChildNodes(const Value: TFsNavTreeNodes);
begin
  FChildNodes.Assign(Value);
end;

procedure TFsNavTreeNode.SetExpanded(const Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
  end;
end;

procedure TFsNavTreeNode.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

{ TFsNavTreeNodes }

constructor TFsNavTreeNodes.Create(AParent: TFsNavTreeNode);
begin
  inherited Create(TFsNavTreeNode);
  FParent := AParent;
end;

constructor TFsNavTreeNodes.Create(ACatalog: TFsNavTreeCatalog);
begin
  inherited Create(TFsNavTreeNode);
  FCatalog := ACatalog;
end;

function TFsNavTreeNodes.GetCatalog: TFsNavTreeCatalog;
begin
  if Assigned(FCatalog) then Result := FCatalog
  else if Assigned(FParent) then Result := FParent.Catalog
  else Result := nil;
end;

procedure TFsNavTreeNodes.Update(Item: TCollectionItem);
var
  lCatalog: TFsNavTreeCatalog;
begin
  inherited;

  lCatalog := Self.Catalog;

  if Assigned(lCatalog) then lCatalog.Changed(False);
end;

{ TFsNavTreeCatalog }

procedure TFsNavTreeCatalog.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TFsNavTreeCatalog then
    Self.FText := TFsNavTreeCatalog(Source).Text
  else inherited;
end;

constructor TFsNavTreeCatalog.Create(Collection: TCollection);
begin
  inherited;
  FNodes := TFsNavTreeNodes.Create(Self);
end;

destructor TFsNavTreeCatalog.Destroy;
begin
  FNodes.Free;
  inherited;
end;

function TFsNavTreeCatalog.GetDisplayName: string;
begin
  Result := FText;
end;

procedure TFsNavTreeCatalog.SetNodes(const Value: TFsNavTreeNodes);
begin
  FNodes.Assign(Value);
end;

procedure TFsNavTreeCatalog.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

{ TFsNavTreeCatalogs }

constructor TFsNavTreeCatalogs.Create(ATree: TFsNavTree);
begin
  inherited Create(TFsNavTreeCatalog);
  FTree := ATree;
end;

procedure TFsNavTreeCatalogs.Update(Item: TCollectionItem);
begin
  inherited;

  if Assigned(FTree) then FTree.Invalidate;
end;

end.
