{
  This file is part of the FastPlaz example.
  (c) Luri Darmawan <luri@fastplaz.com>
  For the full copyright and license information, please view the LICENSE
  file that was distributed with this source code.
}
unit main;

{
  This project is an Example of a Simple Telegram Bot,
  using the Poll method.
  The WebHook method is recommended
}

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler,
  simpleai_controller, fpjson,
  telegram_integration, config_lib, Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, Spin, string_helpers;

const
  _DEVELOPMENT_ = false;
  TELEGRAM_TOKEN = 'telegram/default/token';
  CONFIG_BOTNAME = 'ai/default/name';
  CONFIG_BASEDIR = 'ai/default/basedir';
  CONFIG_ENTITIES = 'ai/default/entities';
  CONFIG_INTENTS = 'ai/default/intents';
  CONFIG_RESPONSE = 'ai/default/response';

type

  { TfMain }

  TfMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    barBottom: TStatusBar;
    btnSendMessage: TBitBtn;
    btnStart: TButton;
    edtMessage: TEdit;
    edtTelegramID: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mem: TMemo;
    memResult: TMemo;
    mainPageControl: TPageControl;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    edtInterval: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tmrPoll: TTimer;
    TrayIcon1: TTrayIcon;
    procedure btnSendMessageClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure edtTelegramIDEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure tmrPollTimer(Sender: TObject);
  private
    baseDir: string;
    Telegram: TTelegramIntegration;
    inProcess: boolean;
    NLP: TSimpleAI;

    procedure loadNLPData;
    procedure onMessageHandler(AMessage: string; var AReply: string;
      var AHandled: boolean);
  public

  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure Delay(AMiliSeconds: DWORD);
var
  DW: DWORD;
begin
  DW := GetTickCount64;
  while (GetTickCount64 < DW + AMiliSeconds) and (not Application.Terminated) do
    Application.ProcessMessages;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  baseDir := '';
  Config.ValidateFile(baseDir + 'config.json');

  // Prepare Telegram Bot
  Telegram := TTelegramIntegration.Create;
  Telegram.Token := Config[TELEGRAM_TOKEN];
  Telegram.OnMessage := @onMessageHandler;

  // NLP Engine
  // NLP data at folder 'files/nlp/*'
  NLP := TSimpleAI.Create;
  NLP.TrimMessage := False;
  NLP.AIName := Config[CONFIG_BOTNAME];
  NLP.Debug := False;
  loadNLPData;

  inProcess := False;
  mem.Align := alClient;
  memResult.Align := alClient;
  mainPageControl.ActivePage := TabSheet1;

  mem.Font.Name := 'Courier';
  mem.Font.Size := 11;
  memResult.Font.Name := 'Courier';
  memResult.Font.Size := 11;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  NLP.Free;
  Telegram.Free;
end;

procedure TfMain.btnStartClick(Sender: TObject);
begin
  if Telegram.Token.IsEmpty then
  begin
    mem.Lines.Add('Token invalid. Check file config.json');
    Exit;
  end;

  tmrPoll.Interval := edtInterval.Value;
  tmrPoll.Enabled := not tmrPoll.Enabled;
  edtInterval.Enabled := not tmrPoll.Enabled;
  if tmrPoll.Enabled then
  begin
    mainPageControl.ActivePage := TabSheet1;
    mem.Lines.Add('== Polling starts every ' + IntToStr(edtInterval.Value) +
      ' miliseconds.');
    btnStart.Caption := '&Stop';
    btnStart.Color := clRed;
    barBottom.Panels[2].Text := 'Service running ...';
  end
  else
  begin
    btnStart.Caption := '&Start';
    btnStart.Color := clGreen;
    barBottom.Panels[2].Text := '';
  end;
end;

procedure TfMain.edtTelegramIDEnter(Sender: TObject);
begin
  if mem.SelText.IsNumeric then
    edtTelegramID.Text := mem.SelText;
end;

procedure TfMain.btnSendMessageClick(Sender: TObject);
begin
  if edtTelegramID.Text = '' then
  begin
    edtTelegramID.SetFocus;
    Exit;
  end;
  if edtMessage.Text = '' then
  begin
    edtMessage.SetFocus;
    Exit;
  end;
  Telegram.SendMessage(edtTelegramID.Text, edtMessage.Text); //without thread
  memResult.Lines.Add(Telegram.ResultText);
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if not _DEVELOPMENT_ then
    Exit;
  if key = 27 then
  begin
    key := 0;
    Close;
  end;
end;

procedure TfMain.FormShow(Sender: TObject);
begin

end;

procedure TfMain.tmrPollTimer(Sender: TObject);
begin
  Telegram.getUpdatesDynamic();
end;

procedure TfMain.loadNLPData;
var
  i: integer;
  s, nlpDir: String;
  jData: TJSONData;
begin
  nlpDir := baseDir + Config[CONFIG_BASEDIR];

  // load Entities
  s := Config[CONFIG_ENTITIES];
  jData := GetJSON(s);
  if jData.Count > 0 then
    for i := 0 to jData.Count - 1 do
    begin
      NLP.AddEntitiesFromFile(nlpDir + jData.Items[i].AsString);
    end;
  jData.Free;

  // load Intents
  s := Config[CONFIG_INTENTS];
  jData := GetJSON(s);
  if jData.Count > 0 then
    for i := 0 to jData.Count - 1 do
    begin
      NLP.AddIntentFromFile(nlpDir + jData.Items[i].AsString);
    end;
  jData.Free;

  // load Response
  s := Config[CONFIG_RESPONSE];
  jData := GetJSON(s);
  if jData.Count > 0 then
    for i := 0 to jData.Count - 1 do
    begin
      NLP.AddResponFromFile(nlpDir + jData.Items[i].AsString);
    end;
  jData.Free;

end;

procedure TfMain.onMessageHandler(AMessage: string; var AReply: string;
  var AHandled: boolean);
var
  s: string;
begin
  // example: using 'if'
  //if AMessage = 'hi' then
  //  AReply := 'hi juga';

  // Process Your Message here
  if NLP.Exec(AMessage) then
  begin
    AReply := NLP.ResponseText.Text.Trim;
    AHandled := True;
  end;

  memResult.Lines.Add(Telegram.RequestContent);
  s := FormatDateTime('yyyy/mm/dd HH:nn:ss', Now) + ' | '
    + Telegram.UserID
    + ':' + Telegram.FullName
    + ' | ' + AMessage + ' » '
    + AReply;

  mem.Lines.Add(s);

end;

//Delay(250);
//Telegram.SendMessageAsThread(Telegram.ChatID, 'recho: ' + AMessage);


end.



