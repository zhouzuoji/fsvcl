unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Grids,
  Dialogs, FSGraphics, FSStdCtrls, StdCtrls, ExtCtrls, ComCtrls, FSScrollControls, FSVclBase, FSNavTree,
  FsControls;

type
  TForm1 = class(TForm)
    FsImage1: TFsImage;
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
    TabSheet5: TTabSheet;
    ListView1: TListView;
    TabSheet6: TTabSheet;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Edit3: TEdit;
    procedure FormCreate(Sender: TObject);
  public
    procedure InitListView;
    procedure InitTreeView;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitListView;
  InitTreeView;
end;

procedure TForm1.InitListView;
var
  i, j: Integer;
  li: TListItem;
begin
  FsListView1.Columns.Clear;

  for i := 1 to 20 do
    FsListView1.Columns.Add.Caption := 'column' + IntToStr(i);

  FsListView1.Items.BeginUpdate;

  try
    for i := 1 to 1000 do
    begin
      li := FsListView1.Items.Add;
      li.Caption := 'No' + IntToStr(i);

      for j := 2 to 20 do
        li.SubItems.Add('row' + IntToStr(i) + ', col' + IntToStr(j));
    end;
  finally
    FsListView1.Items.EndUpdate;
  end;

  ListView1.Columns.Clear;

  for i := 1 to 20 do
    ListView1.Columns.Add.Caption := 'column' + IntToStr(i);

  ListView1.Items.BeginUpdate;

  try
    for i := 1 to 1000 do
    begin
      li := ListView1.Items.Add;
      li.Caption := 'No' + IntToStr(i);

      for j := 2 to 20 do
        li.SubItems.Add('row' + IntToStr(i) + ', col' + IntToStr(j));
    end;
  finally
    ListView1.Items.EndUpdate;
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

end.
