        {********************************************************}
        {                                                        }
        {       Read and send data to Yuneec CGO3+ camera        }
        {                                                        }
        {       Copyright (c) 2025         Helmut Elsner         }
        {                                                        }
        {       Compiler: FPC 3.2.3   /    Lazarus 3.7           }
        {                                                        }
        { Pascal programmers tend to plan ahead, they think      }
        { before they type. We type a lot because of Pascal      }
        { verboseness, but usually our code is right from the    }
        { start. We end up typing less because we fix less bugs. }
        {           [Jorge Aldo G. de F. Junior]                 }
        {********************************************************}

(*
This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.


THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*******************************************************************************)

{App purpose: Control a C23/E90 camera to use it for other purposes.

This Demo shows how to control the gimbal of a C23 or E90 camera (and possibly
all cameras that fits H Plus, H3 or H520). You can do all what ST16S can do,
tilt, pan, tilt mode, pan mode.
The charts on the right showing the results sent by MAV message MOUNT_ORIENTATION.


Gimbal control is done with a Yuneec specific MAV message, MsgID 5000.
The CRC_EXTRA for this message is 252.

This unit needs following additional components:
- Synapse

Also the units mav_def and mav_msg from repository "Common units" are needed:
https://github.com/h-elsner/common_units

}

unit CGO3read_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  lclintf, lcltype, Buttons, ActnList, Process, XMLPropStorage, ComCtrls,
  TAGraph, TASeries, TAChartUtils, synaser, MKnob, Grids, mav_def, mav_msg;

type

  { TForm1 }

  TForm1 = class(TForm)
    acConnect: TAction;
    acClose: TAction;
    acDisconnect: TAction;
    acScanPorts: TAction;
    ActionList1: TActionList;
    btnDisconnect: TBitBtn;
    btnClose: TBitBtn;
    btnConnect: TBitBtn;
    btnCenter: TButton;
    btnFrontCali: TButton;
    cbPort: TComboBox;
    cbRecord: TCheckBox;
    cbSpeed: TComboBox;
    ImageList1: TImageList;
    chPanLineSeries1: TLineSeries;
    chRoll: TChart;
    chRollLineSeries1: TLineSeries;
    chTilt: TChart;
    chPan: TChart;
    chTiltLineSeries1: TLineSeries;
    knPanControl: TmKnob;
    lblBootTimeOut: TLabel;
    lblBootTime: TLabel;
    lblPanControl: TLabel;
    pcMain: TPageControl;
    rgPanMode: TRadioGroup;
    rgTiltMode: TRadioGroup;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    gridAttitude: TStringGrid;
    timerFCCommand: TTimer;
    timerFCHeartbeat: TTimer;
    tbTiltControl: TTrackBar;
    tsFC: TTabSheet;
    upperPanel: TPanel;
    XMLPropStorage1: TXMLPropStorage;

    procedure acCloseExecute(Sender: TObject);
    procedure acConnectExecute(Sender: TObject);
    procedure acDisconnectExecute(Sender: TObject);
    procedure acScanPortsExecute(Sender: TObject);
    procedure btnCenterClick(Sender: TObject);
    procedure cbPortDblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure knPanControlChange(Sender: TObject; AValue: Longint);
    procedure timerFCCommandTimer(Sender: TObject);
    procedure timerFCHeartbeatTimer(Sender: TObject);

  private
    procedure StopAllTimer;
    procedure FillCharts;
    procedure GUIsetCaptionsAndHints;
    procedure NumberMessagesInStatusBar;
    procedure ActAsFlightController(var msg: TMAVmessage; list: TStringList);

  public
    procedure CreateE90GimbalControlMessage(var msg: TMavMessage; const SequenceNumber: byte);
    procedure CreateE90ControlHeartbeatMessage(var msg: TMAVmessage; const SequenceNumber: byte );
    procedure ReadMessages_FD(var msg: TMAVmessage);
    procedure RecordMessageAsCSV(msg: TMAVmessage; list: TStringList; LengthFixPart: byte);
  end;

  {$I E90Demo_en.inc}

var
  Form1: TForm1;
  UART: TBlockSerial;
  UARTConnected: boolean;
  starttime: UInt64;
  SequNumberTransmit: byte;
  MessagesSent, MessagesReceived: integer;
  mount: TAttitudeData;

const
  AppVersion='V0.4 2024-03-05';
  linkLazarus='https://www.lazarus-ide.org/';
  tab1=' ';
  tab2='  ';
  csvseparator=';';

  maxPorts=10;
  timeout=100;
  defaultbaud=500000;

{$IFDEF WINDOWS}
  default_port='COM6';
{$ELSE}                                                {UNIX like OS}
  default_port='/dev/ttyUSB0';
{$ENDIF}


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  UARTconnected:=false;
  GUIsetCaptionsAndHints;
end;

procedure TForm1.GUIsetCaptionsAndHints;
begin
  Caption:=Application.Title+tab2+AppVersion;
  cbSpeed.Text:=IntToStr(defaultbaud);
  cbSpeed.Hint:=hntSpeed;
  cbPort.Hint:=hntPort;
  acScanPorts.Caption:=capPort;
  acScanPorts.Hint:=hntPort;
  acConnect.Caption:=capConnect;
  acConnect.Hint:=hntConnect;
  acDisConnect.Caption:=capDisConnect;
  acDisConnect.Hint:=hntDisConnect;
  acClose.Caption:=capClose;
  btnClose.Hint:=hntClose;

  btnCenter.Caption:=capCenter;
  btnCenter.Hint:=hntCenter;

  StatusBar1.Hint:=hntStatusBar;
  tbTiltControl.Hint:=hntTiltControl;
  rgTiltMode.Hint:=hntTiltMode;
  rgPanMode.Hint:=hntPanMode;
  knPanControl.Hint:=hntPanControl;

  cbRecord.Caption:=capRecord;
  cbRecord.Hint:=hntRecord;
  tsFC.Caption:=captsFC;
  lblBootTime.Caption:=capBootTime;
  lblBootTime.Hint:=hntBootTime;
  lblBootTimeOut.Caption:='';

  gridAttitude.Cells[0, 0]:='Gimbal';
  gridAttitude.Cells[1, 0]:=rsvalue+' [°]';
  gridAttitude.Cells[0, 1]:='Tilt';
  gridAttitude.Cells[0, 2]:='Pan';
  gridAttitude.Cells[0, 3]:='Roll';
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  StopAllTimer;
  acScanPortsExecute(self);
  btnConnect.SetFocus;
end;

procedure TForm1.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.StopAllTimer;
begin
  timerFCHeartbeat.Enabled:=false;
  timerFCCommand.Enabled:=false;
end;

procedure SetStartValuesForGlobelVariables;
begin
  SequNumberTransmit:=0;
  MessagesSent:=0;
  MessagesReceived:=0;
  starttime:=GetTickCount64;
end;

function SendUARTMessage(const msg: TMAVmessage; LengthFixPart: byte): boolean;
begin
  result:=false;
  if msg.valid then begin
    if UART.SendBuffer(@msg.msgbytes, msg.msglength+LengthFixPart+2)>LengthFixPart then begin
      result:=true;
      inc(MessagesSent);
    end;
  end;
end;

function InvertPanControlPosition(pos: uint16): uint16;
begin
  result:=4096-pos;
end;

procedure WriteCSVRawHeader(var list: TStringList);
var
  s: string;
  i: integer;

begin
  list.Clear;
  s:=rsTime;
  for i:=0 to 50 do
    s:=s+csvseparator+Format('%.*d', [2, i]);
  list.Add(s);
end;

procedure TForm1.CreateE90ControlHeartbeatMessage(var msg: TMAVmessage; const SequenceNumber: byte );
begin
  CreateE90StandardPartMsg(msg, 9);
  msg.msgbytes[4]:=SequenceNumber;
  msg.msgbytes[12]:=4;
  msg.msgbytes[13]:=3;
  msg.msgbytes[14]:=$0D;
  msg.msgbytes[15]:=$0C;
  msg.msgbytes[16]:=$1D;
  msg.msgbytes[17]:=3;
  msg.msgbytes[18]:=3;
  SetCRC(msg, LengthFixPartFD, CRC_EXTRA_heartbeat);
end;

procedure TForm1.CreateE90GimbalControlMessage(var msg: TMavMessage; const SequenceNumber: byte);
var
  panmode, tiltmode: uint16;

begin
  CreateE90StandardPartMsg(msg, $1A);
  msg.msgbytes[4]:=SequenceNumber;
  msg.msgbytes[6]:=1;                                  {CompID}
  SetUInt16ToMsg(msg, 7, 5000);                        {MsgID GimbalControl 5000}

  if rgTiltMode.ItemIndex=1 then
    tiltmode:=3000
  else
    tiltmode:=2100;
  if rgPanMode.ItemIndex=2 then
    panmode:=3000
  else
    panmode:=830;
  if rgPanMode.ItemIndex=0 then
    SetUInt16ToMsg(msg, 24, 2048)
  else
    SetUInt16ToMsg(msg, 24, InvertPanControlPosition(knPanControl.Position));
  SetUInt16ToMsg(msg, 26, tbTiltControl.Position);

  SetUInt16ToMsg(msg, 28, 2048);
  SetUInt16ToMsg(msg, 30, panmode);
  SetUInt16ToMsg(msg, 32, tiltmode);
  SetUInt16ToMsg(msg, 34, 2048);
  SetCRC(msg, LengthFixPartFD, CRC_EXTRA_cmd5000);
end;

function ConnectUART(port, speed: string): string;
begin
  result:='';
  if UARTconnected then
    exit;
  UART:=TBlockSerial.Create;
  {$IFDEF LINUX}
    UART.LinuxLock:=false;
  {$ENDIF}
  UART.Connect(port);
  sleep(200);
  UART.Config(StrToIntDef(speed, defaultbaud), 8, 'N', SB1, false, false);
  if UART.LastError=0 then begin
    UARTConnected:=true;
    result:='Status: '+UART.LastErrorDesc;
  end else begin
    result:='Error: '+UART.LastErrorDesc;
  end;
end;

procedure DisconnectUART;
begin
  if UARTConnected then begin
    try
      UART.CloseSocket;
    finally
      UART.Free;
      UARTConnected:=false;
    end;
  end;
end;

procedure TForm1.NumberMessagesInStatusBar;
begin
  StatusBar1.Panels[0].Text:='S: '+IntToStr(MessagesSent);
  StatusBar1.Panels[1].Text:='R: '+IntToStr(MessagesReceived);
end;

procedure TForm1.acConnectExecute(Sender: TObject);
var
  msg: TMAVmessage;
  csvlist: TStringList;

begin
  csvlist:=TStringList.Create;
  try
    msg:=Default(TMAVmessage);
    SetStartValuesForGlobelVariables;
    chTiltLineSeries1.Clear;
    chPanLineSeries1.Clear;
    chRollLineSeries1.Clear;
    StatusBar1.Panels[0].Text:='0';                    {Sent messages}
    StatusBar1.Panels[1].Text:='0';                    {Received messages}
    StatusBar1.Panels[2].Text:=ConnectUART(cbPort.Text, cbSpeed.Text);
    WriteCSVRawHeader(csvlist);

    If UARTconnected then begin
      StatusBar1.Panels[2].Text:=StatusBar1.Panels[2].Text+'  -  '+rsConnected;
      ActAsFlightController(msg, csvlist);
      NumberMessagesInStatusBar;
      SaveDialog1.FilterIndex:=1;
      SaveDialog1.FileName:='FDmessages_'+FormatDateTime('yyyymmdd_hhnnss', now)+'.csv';

      if cbRecord.Checked and (csvlist.Count>1) and SaveDialog1.Execute then begin
        csvlist.SaveToFile(SaveDialog1.FileName);
        StatusBar1.Panels[2].Text:=SaveDialog1.FileName+rsSaved;
      end;
    end;
  finally
    csvlist.Free;
  end;
end;

procedure TForm1.RecordMessageAsCSV(msg: TMAVmessage; list: TStringList; LengthFixPart: byte);
var
  s: string;
  i: integer;

begin
  s:=FormatFloat(floatformat3, (GetTickCount64-starttime)/1000);
  for i:=0 to msg.msglength+LengthFixPart+1 do begin
    s:=s+csvseparator+IntToHex(msg.msgbytes[i], 2);
  end;
  list.Add(s);
end;

procedure TForm1.FillCharts;
begin
  lblBootTimeOut.Caption:=FormatDateTime(timeHzzz, mount.boottime);
  chTiltLineSeries1.AddXY(MessagesReceived, mount.pitch);
  chPanLineSeries1.AddXY(MessagesReceived, mount.yaw);
  chRollLineSeries1.AddXY(MessagesReceived, mount.roll);

  gridAttitude.Cells[1, 1]:=FormatFloat(floatformat2, mount.pitch);
  gridAttitude.Cells[1, 2]:=FormatFloat(floatformat2, mount.yaw);
  gridAttitude.Cells[1, 3]:=FormatFloat(floatformat3, mount.roll);
end;

function FormatBootTime(const data: TGPSdata): string;
begin
  result:=FormatDateTime(timezzz, data.boottime);
end;

procedure TForm1.ActAsFlightController(var msg: TMAVmessage; list: TStringList);
begin
  mount:=default(TAttitudeData);
  timerFCHeartbeat.Enabled:=true;
  sleep(100);
  timerFCcommand.Enabled:=true;
  while (UART.LastError=0) and UARTConnected do begin
    if UART.CanRead(0) then begin
      ReadMessages_FD(msg);
      if msg.valid then begin
        if msg.msgid32=$109 then begin
          MOUNT_ORIENTATION(msg, LengthFixPartFD, mount);
          FillCharts;
        end;
        if cbRecord.Checked then
          RecordMessageAsCSV(msg, list, LengthFixPartFD);
        inc(MessagesReceived);
      end;
    end;
    Application.ProcessMessages;
  end;
end;

procedure TForm1.ReadMessages_FD(var msg: TMAVmessage);
var
  b: byte;
  i: integer;

begin
  msg.valid:=false;
  repeat
    b:=UART.RecvByte(timeout);
  until (b=MagicFD) or (UART.LastError<>0) or (not UARTConnected);
  msg.msgbytes[0]:=b;

  msg.msglength:=UART.RecvByte(timeout);
  msg.msgbytes[1]:=msg.msglength;
  for i:=2 to msg.msglength+LengthFixPartFD+1 do
    msg.msgbytes[i]:=UART.RecvByte(timeout);
  msg.msgid32:=MAVgetUInt32(msg, 7) and $FFFFFF;

  if CheckCRC16MAV(msg, LengthFixPartFD, 1, true, GetCRCextra(msg.msgid32)) then begin
    msg.sysid:=msg.msgbytes[5];
    msg.targetid:=msg.msgbytes[6];
    msg.valid:=true;
  end;
end;

procedure TForm1.acDisconnectExecute(Sender: TObject);
begin
  StopAllTimer;
  DisconnectUART;
  StatusBar1.Panels[2].Text:=rsDisconnected;
end;

procedure TForm1.acScanPortsExecute(Sender: TObject);
var
{$IFDEF UNIX}
  cmd: TProcess;
  list: TStringList;
{$ENDIF}
  i: integer;

begin
{$IFDEF WINDOWS}
  cbPort.Text:='';
  cbPort.Items.Clear;
  cbPort.Items.CommaText:=GetSerialPortNames;
  if cbPort.Items.Count>0 then begin
    cbPort.Text:=cbPort.Items[cbPort.Items.Count-1];
    StatusBar1.Panels[2].Text:=cbPort.Items[cbPort.Items.Count-1];
  end else
    StatusBar1.Panels[2].Text:=errNoUSBport;

{$ENDIF}
{$IFDEF UNIX}
  cmd:=TProcess.Create(nil);
  list:=TStringList.Create;
  try
    cmd.Options:=cmd.Options+[poWaitOnExit, poUsePipes];
    cmd.Executable:='ls';
    for i:=0 to cbPort.Items.count-1 do begin
      cmd.Parameters.Clear;
      cmd.Parameters.Add(cbPort.Items[i]);
      cmd.Execute;
      list.LoadFromStream(cmd.Output);
      if list.Count>0 then begin
        StatusBar1.Panels[2].Text:=list[0];
      end;
    end;
  finally
    cmd.Free;
    list.Free;
  end;
{$ENDIF}
end;

procedure TForm1.btnCenterClick(Sender: TObject);
begin
  knPanControl.Position:=2048;
end;

procedure TForm1.cbPortDblClick(Sender: TObject);
begin
  acScanPortsExecute(self);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DisconnectUART;
end;

procedure TForm1.knPanControlChange(Sender: TObject; AValue: Longint);
begin
  lblPanControl.Caption:=IntToStr(InvertPanControlPosition(knPanControl.Position));
end;

procedure TForm1.timerFCHeartbeatTimer(Sender: TObject);
var
  msg: TMAVmessage;

begin
  if UARTConnected then begin
    CreateE90ControlHeartbeatMessage(msg, SequNumberTransmit);
    if SendUARTMessage(msg, LengthFixPartFD) then
      Inc(SequNumberTransmit);
  end;
  NumberMessagesInStatusBar;
end;

procedure TForm1.timerFCCommandTimer(Sender: TObject);
var
  msg: TMAVmessage;

begin
  if UARTConnected then begin
    CreateE90GimbalControlMessage(msg, SequNumberTransmit);
    if SendUARTMessage(msg, LengthFixPartFD) then
      Inc(SequNumberTransmit);
  end;
end;

end.

