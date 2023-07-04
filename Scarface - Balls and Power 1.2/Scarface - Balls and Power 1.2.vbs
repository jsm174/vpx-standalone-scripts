'**************far*******************
' Scarface - Balls and Power v 1.2
'      VP table by JPSalas
'  Graphics & Rules by hassanchop
'*********************************

'**************far*************
'DOF version by Arngrim v 1.1

'DOF mapping

'E107 bumper right
'E108 ball release
'E109 middle yellow target left
'E110 middle yellow target center
'E111 middle yellow target right
'E112 middle yellow targets drop reset
'E113 B of Bank target
'E114 A of Bank target
'E115 N of Bank target
'E116 K of Bank target
'E117 Central bank hit
'E118 World is yours VUK
'E119 Car eject
'E120 Left Ramp done
'E121 Center door open
'E122 Platform rotating
'E123 Autoplunger
'E124 Plunger lane exit
'E125 Plunger lane diverter
'E126 Car hit
'E127 Shooter lane switch
'E128 Left outer lane
'E129 Left inner lane
'E130 Right inner lane
'E131 Right outer lane
'E132 Start button light
'E133 Right ramp done
'E134 Knocker
'E135 Lockpost
'E136 Backdoorpost
'E137 Win event

'E201 rollover middle left
'E202 rollover middle right
'E203 rollover left
'E204 rollover right
'E205 rotation lights from platform 1
'E206 rotation lights from platform 2
'E207 rotation lights from platform 3
'E208 rotation lights from platform 4
'E209 rotation lights from platform 5
'E210 flash when skillshot made
'E211 
'E212 
'E213 
'E214 Bumper flash
'E219 left lane light
'E222 Easter egg beacon

'**************far***********

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

'************************************** B2S additions / Rosve **************************************

Const FullBackglassFX=1  'When set to 0 some backglass effects are skipped. Change to 1 if you have a fast PC.
Const LWMaxBallsOnPF=1   'Some flasher effects are skipped if there are more than LWMaxBallsOnPF in play.
                         ' This setting can be increased on a fast PC.  

' B2S *******************************************************************************
Const cGameName = "scarface"

' some B2S vars
Dim B2SCurrentFrame

' initialize this vars
B2SCurrentFrame = ""

' B2S *******************************************************************************

' some constants
Const BallSavedTime = 20 'in seconds
Const MaxMultiplier = 10
Const BallsPerGame = 3
Const MaxBalls = 5                  'Max number of balls on the table at the same time
Const MusicOn = True
Dim Version:Version = chr(193) & "0" 'chr(193) is "1,"

Const BallSize = 25  'Ball radius


Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components and DT DMD
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=1
dim xxx
For each xxx in DMDElements:xxx.Visible = 1: Next
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive13.visible=0
For each xxx in DMDElements:xxx.Visible = 0: Next
End if

'********************
' Table Init & Events
'********************

Dim GameStarted
Dim Tilt
Dim Tilted
Dim TiltWarning
Dim AutoPlungerReady
Dim BallsRemaining
Dim BallsOnPlayfield
Dim Special
Dim SpecialIsLit
Dim SpecialAwarded
Dim Match
Dim LastSwitchHit
Dim BallLockEnabled
Dim BallsInLock
Dim BallSaved
Dim ExtraBallIsLit
Dim ExtraBallAwarded
Dim MultiBallMode
Dim Credits
Dim Player
Dim Players
Dim BonusAward

Sub Table1_Init()
	LoadEM
    GameStarted = 0
    Credits = 0
    Player = 0
    Players = 0
    Loadhs
    GiOff
    AllLampsOff
    DMDInit

    ' Init bumpers, slingshots, targets and other VP objects
    LockPost.Isdropped = 1:
	BackDoorPost.Isdropped = 1

    GameTimer.Enabled = 1
    AutoPlunger.Pullback
	If Credits > 0 Then
		DOF 132, DOFOn
	End If
    ' Start Attrack mode
    AttractMode_On
    GiOn
End Sub

Sub Table1_Exit()
	If B2SOn Then
		Controller.Stop
	End If
End Sub

Sub Initialize   ' First Ball
    ResetLights
    Score(0) = 0 'used in the EE
    Score(1) = 0
    Score(2) = 0
    Score(3) = 0
    Score(4) = 0
    Bonus = 0
    BallsRemaining = BallsPerGame + 1 ' first ball
    BallsOnPlayfield = 0
    BallLockEnabled = 0
    BallsInLock = 0
    BallsInDesk = 0
    BallSaved = 0
    BonusMultiplier = 1
    MultiballDelay = 0
    NextBallBusy = 0
    TiltDelay = 0
    Tilted = False
    Tilt = 0
    TiltWarning = 0
    Special = 0
    SpecialIsLit = 0
    SpecialAwarded = 0
    AutoPlungerDelay = 0
    AutoPlungerReady = 0
    MultiBallMode = 0
    ExtraBallIsLit = 0
    ExtraBallAwarded = 0
    GameStarted = 1
    Goal1 = 2000000
    Goal2 = 6000000
    Made1(1) = False
    Made1(2) = False
    Made1(3) = False
    Made1(4) = False
    Made2(1) = False
    Made2(2) = False
    Made2(3) = False
    Made2(4) = False
    PlaySong "bgout_scarface_NOE.mp3"
    ' Reset Variables
    InitVariables
    ' Init Modes
    ResetDroptargets
    InitModes
    InitTV
    InitSkillShot
    InitLaneLights
    InitExtraBall
    InitLock
    InitYeyo
    InitBumpers
    InitScarface
    InitKillRamp
    InitGetCar
    InitMiniLoops
    InitTWIY
    InitEvents
    InitEasterEgg
End Sub

Sub StartGame
    If B2SOn Then Controller.B2SStopAllAnimations()
    Initialize
    AttractMode_Off
    GiOn
    DisPlayScoreNow
    NextBallDelay = 30
End Sub

Sub ResetLights
    AllLampsOff
End Sub

'******************************
'     Game Timer Loop
' used for all the small waits
'******************************

Dim BallRelDelay, BonusDelay, NextBallDelay, TiltDelay
Dim GameOverDelay, MatchDelay, MultiballDelay, AutoPlungerDelay, EjectLockedBallsDelay, EjectRightBallDelay
Dim ComboDelay, NextBallBusy, VukDelay

Sub GameTimer_Timer
    Dim x
    ' Check some subs - update realtime variables
    'check the delays

    If NextBallDelay> 0 Then
        NextBallDelay = NextBallDelay - 1
        If NextBallDelay = 0 Then
            NextBall
        End If
    End If

    If TiltDelay> 0 Then
        TiltDelay = TiltDelay - 1
        If TiltDelay = 0 Then
            Tilt = Tilt - 1
            If Tilt> 0 Then
                TiltDelay = 20
            Else
                TiltWarning = 0
            End If
        End If
    End If

    If GameOverDelay> 0 Then
        GameOverDelay = GameOverDelay - 1
        If GameOverDelay = 0 Then GameOver
    End If

    If BonusDelay> 0 Then
        BonusDelay = BonusDelay - 1
        If BonusDelay = 39 Then
            'add bonus to score
            BonusMultiplier = EventsFinished 'only for this table
            DD CenterTop("BONUS"), CenterBottom(FormatScore(Bonus) & " X" & BonusMultiplier), "", eNone, eNone, 0, 2000, True, ""
            AddScore Bonus * BonusMultiplier
            If BonusHeld Then
                BonusMultiplier = 1 'reset only the multiplier
            Else
                Bonus = 0:BonusMultiplier = 1
            End If
        End If
        If BonusDelay = 0 Then NextBallDelay = 40:End If
    End If

    If MatchDelay> 0 then 'it works with the two digits of the last player's score
        MatchDelay = MatchDelay - 1

        If MatchDelay = 99 then
            DisplayFlushQueue()
            DD FormatScore(Score(player) ) & "       ", "        30 ", "", eNone, eNone, 0, 150, False, ""
            DD FormatScore(Score(player) ) & "       ", "        70 ", "", eNone, eNone, 0, 150, False, ""
            DD FormatScore(Score(player) ) & "       ", "        20 ", "", eNone, eNone, 0, 150, False, ""
            DD FormatScore(Score(player) ) & "       ", "        60 ", "", eNone, eNone, 0, 150, False, ""
            DD FormatScore(Score(player) ) & "       ", "        90 ", "", eNone, eNone, 0, 150, False, ""
            DD FormatScore(Score(player) ) & "       ", "        10 ", "", eNone, eNone, 0, 150, False, ""
        End If

        If MatchDelay = 0 then
            Match = 10 * (INT(10 * Rnd(1) ) ):If Match <10 Then Match = "00"
            x = Match
            If Match = "00" Then x = 0
            If x = Score(player) MOD 100 Then
                DisplayFlushQueue
                DD FormatScore(Score(player) ) & "       ", "        MATCH " &Match, "", eNone, eBlinkFast, 0, 2000, True, ""
                PlaySound SoundFXDOF("fx_knocker",134,DOFPulse,DOFKnocker)
                Credits = Credits + 1
				If Credits = 1 then
				    DOF 132, DOFOn
				End If
            Else
                DD FormatScore(Score(player) ) & "       ", "        " &Match, "", eNone, eNone, 0, 2000, True, ""
            End If
            GameOverDelay = 60
        End If
    End If

    If BallSavedDelay> 0 Then
        BallSavedDelay = BallSavedDelay - 1
        If BallSavedDelay = 100 Then
            BlinkFastBallSaved 'blink fast the last 5 seconds
        End If
        If BallSavedDelay = 0 Then
            EndBallSaved
        End If
    End If

    '    If ComboDelay> 0 Then
    '        ComboDelay = ComboDelay - 1
    '        If ComboDelay = 0 AND ComboStarted = 1 Then
    '            ResetCombo
    '        End If
    '    End If

    If AutoPlungerDelay> 0 Then
        AutoPlungerDelay = AutoPlungerDelay - 1
        If AutoPlungerDelay = 0 Then
            AutoPlungerFire
            AutoPlungerReady = 0
        End If
    Else
        If BallInPlunger AND MultiBallMode AND EventStarted <> 8 Then AutoPlungerDelay = 1
    End If

    If MultiballDelay> 0 Then ' Eject the balls to the autoplunger
        MultiballDelay = MultiballDelay - 1
        If MultiballDelay = 80 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:End If

        If MultiballDelay = 60 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:End If
        If MultiballDelay = 40 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:End If
        If MultiballDelay = 20 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:End If
        If MultiballDelay = 0 Then MultiBallMode = 1:NewBall:AutoPlungerReady = 1:AutoPlungerDelay = 10:End If
    End If

    ' only for this table

    If PlungerLaneKDelay> 0 Then
        PlungerLaneKDelay = PlungerLaneKDelay - 1
        If PlungerLaneKDelay = 0 Then
            BallsInPlungerLane = BallsInPlungerLane - 1
            PlungerLaneExit
            If BallsInPlungerLane> 0 Then PlungerLaneKDelay = 20
        End If
    End If

    If OpenLockBlockDelay> 0 Then
        OpenLockBlockDelay = OpenLockBlockDelay - 1
        If OpenLockBlockDelay = 0 Then
            OpenLockBlock
        End If
    End If

    If LaneGateDelay> 0 Then
        LaneGateDelay = LaneGateDelay - 1
        If LaneGateDelay = 0 Then
            LaneGateClose
        End If
    End If

    If CarHoleDelay> 0 Then
        CarHoleDelay = CarHoleDelay - 1
        If CarHoleDelay = 0 Then
            CarHoleExit
        End If
    End If

    If CarHole8Delay> 0 Then
        CarHole8Delay = CarHole8Delay - 1
        If CarHole8Delay = 0 Then
            CarHole8Exit
        End If
    End If

    If bumperoffdelay> 0 Then
        bumperoffdelay = bumperoffdelay - 1
        If bumperoffdelay = 0 Then
            InitBumpers
        End If
    End If

    If BankDelay> 0 Then
        BankDelay = BankDelay - 1
        If BankDelay = 0 Then
            EndBank
        End If
    End If

    If GetCarDelay> 0 Then
        GetCarDelay = GetCarDelay - 1
        If GetCarDelay = 0 Then
            EndGetCar
        End If
    End If

    If StartTWIYDelay> 0 Then
        StartTWIYDelay = StartTWIYDelay - 1
        If StartTWIYDelay = 0 Then
            StartTWIY
        End If
    End If

    If Event1Delay> 0 Then
        Event1Delay = Event1Delay - 1
        If Event1Delay = 400 Then Say "doing_a_great_job", "13252527":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event1Delay = 300 Then Say "wastemytime", "13121324461":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event1Delay = 200 Then Say "You_fuckin_hossa", "13172617171":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event1Delay = 0 Then
            EndMainEvent
        End If
    End If

    If Event2Delay> 0 Then
        Event2Delay = Event2Delay - 1
        If Event2Delay = 700 Then Say "No_fuckin_way", "1314251":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event2Delay = 300 Then Say "Ok_your_starting_to_piss_me_off", "222514122315421":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event2Delay = 0 Then
            EndMainEvent
        End If
    End If

    If Event3Delay> 0 Then
        Event3Delay = Event3Delay - 1
        If Event3Delay = 700 Then Say "Thats_ok", "26351":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event3Delay = 400 Then Say "Ok", "32341":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event3Delay = 300 Then Say "Ok_so_what_you_doing_later", "3223841214131":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event3Delay = 0 Then
            EndMainEvent
        End If
    End If

    If Event4Delay> 0 Then
        Event4Delay = Event4Delay - 1
        If Event4Delay = 0 Then
            EndMainEvent
        End If
    End If

    If Event5Delay> 0 Then
        Event5Delay = Event5Delay - 1
        If Event5Delay = 700 Then Say "Hello_pussy_cat", "16242424":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event5Delay = 400 Then Say "Hey_there_sweet_cheeks", "2414131314":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event5Delay = 300 Then Say "Cmon_pussy_cat", "16272538":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event5Delay = 0 Then
            EndMainEvent
        End If
    End If

    If Event6Delay> 0 Then
        Event6Delay = Event6Delay - 1
        If Event6Delay = 700 Then Say "You_fuckin_kidding_me_man", "2324133413141":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event6Delay = 600 Then Say "Dont_you_fucken_listen_man", "2313432613131336":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event6Delay = 400 Then Say "Fuckin_cock_sucker", "8524252524":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event6Delay = 300 Then Say "For_fuck_sake", "231323241":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event6Delay = 150 Then Say "Fuck_you_chico", "16574539":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event6Delay = 0 Then
            EndMainEvent
        End If
    End If

    If Event7Delay> 0 Then
        Event7Delay = Event7Delay - 1
 
        If Event7Delay = 400 Then Say "hello1", "391":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event7Delay = 300 Then Say "hello2", "281":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event7Delay = 0 Then
            EndMainEvent
        End If
    End If

    If Event8Delay> 0 Then
        Event8Delay = Event8Delay - 1
        If Event8Delay = 400 Then Say "Bring_my_car_dont_fuck_around", "179517972636663925":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event8Delay = 200 Then Say "I_need_my_fuckin_car_man", "162424161476371":If B2SOn Then Controller.B2SStartAnimation("Tony")
    End If

    If Event9Delay> 0 Then
        Event9Delay = Event9Delay - 1
        If Event9Delay = 700 Then Say "I_piss_in_your_face", "2333131313131313132":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event9Delay = 400 Then Say "Im_Tony_fuckin_Montana", "13362526251":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event9Delay = 300 Then Say "Now_your_fucked", "26361":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event9Delay = 0 Then
            EndMainEvent
        End If
    End If

    If StartEvent11Delay> 0 Then
        StartEvent11Delay = StartEvent11Delay - 1
        If StartEvent11Delay = 0 Then
            StartEvent11
        End If
    End If

    If Event11VoiceDelay> 0 Then
        Event11VoiceDelay = Event11VoiceDelay - 1
        If Event11VoiceDelay = 800 Then Say "You_know_who_your_fuckin_with", "25162424261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event11VoiceDelay = 700 Then Say "You_need_an_army_to_take_me", "13151414141413331":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event11VoiceDelay = 600 Then SayEvent11
        If Event11VoiceDelay = 500 Then Say "You_picked_the_wrong_guy_to_fuck_with", "23232523233334261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event11VoiceDelay = 400 Then Say "Cmon_bring_your_army", "13144814":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event11VoiceDelay = 300 Then SayEvent11
        If Event11VoiceDelay = 200 Then Say "You_think_you_can_fuck_with_me", "2414141423261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event11VoiceDelay = 100 Then Say "Cmon_come_and_get_me", "12132643233414":If B2SOn Then Controller.B2SStartAnimation("Tony")
        If Event11VoiceDelay = 0 Then
            SayEvent11
            Event11VoiceDelay = 800
        End If
    End If

    If EndEvent11Delay> 0 Then
        EndEvent11Delay = EndEvent11Delay - 1
        If EndEvent11Delay = 0 Then
            EndEvent11
        End If
    End If

    If EndWinEvent11Delay> 0 Then
        EndWinEvent11Delay = EndWinEvent11Delay - 1
        If EndWinEvent11Delay = 0 Then
            EndWinEvent11
        End If
    End If

    If ResetDropDelay> 0 Then
        ResetDropDelay = ResetDropDelay - 1
        If ResetDropDelay = 0 Then
            ResetDroptargets
        End If
    End If

    If VukDelay> 0 Then
        VukDelay = VukDelay - 1
        If VukDelay = 20 Then
            PlaySound "reload3_shotgun"
        End If
        If VukDelay = 0 Then
            VukEject
        End If
    End If

    If EndEEDelay> 0 Then
        EndEEDelay = EndEEDelay - 1
        If EndEEDelay = 0 Then
            EndEE
        End If
    End If

    If EndEEDelay2> 0 Then
        EndEEDelay2 = EndEEDelay2 - 1
        If EndEEDelay2 = 0 Then
            If BallsOnPlayfield Then
                EndEEDelay2 = 10
            Else
                GameOver_Part2
            End If
        End If
    End If

    If StartEasterEggShootingDelay> 0 Then
        StartEasterEggShootingDelay = StartEasterEggShootingDelay - 1
        If StartEasterEggShootingDelay = 0 Then
            StartEasterEggShooting
        End If
    End If

    If StartHitManDelay> 0 Then
        StartHitManDelay = StartHitManDelay - 1
        If StartHitManDelay = 0 Then
            StartHitMan2
        End If
    End If

    If CTDelay> 0 Then
        CTDelay = CTDelay - 1
        If CTDelay = 90 Then l40.State = 0:l38.State = 1:CurrentCT = 1
        If CTDelay = 60 Then l38.State = 0:l39.State = 1:CurrentCT = 2
        If CTDelay = 30 Then l39.State = 0:l40.State = 1:CurrentCT = 3
        If CTDelay = 0 Then CTDelay = 91
    End If

    If EjectXLockedBallDelay> 0 Then 'eject extra ball
        EjectXLockedBallDelay = EjectXLockedBallDelay - 1
        If EjectXLockedBallDelay = 0 Then
            BallsInLock = BallsInLock - 1
            BallsOnPlayfield = BallsOnPlayfield + 1
            LockExit
        End If
    End If

    If EjectBallsInDeskDelay> 0 Then 'eject balls from under the desk
        EjectBallsInDeskDelay = EjectBallsInDeskDelay - 1
        If EjectBallsInDeskDelay = 0 Then
            If BallsInDesk> 0 Then
                BallsInDesk = BallsInDesk - 1
                BallsOnPlayfield = BallsOnPlayfield + 1
                LockExit
            End If
            If BallsInDesk> 0 Then EjectBallsInDeskDelay = 20
        End If
    End If
End Sub

'*****
'Drain
'*****

Sub Drain_Hit
    
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield -1
    PlaySoundat "fx_drain", drain
    LastSwitchHit = "drain"

    ' end modes

    If Tilted Then
        If BallsOnPlayfield = 0 Then
            Tilted = false:TiltObjects 0
            LightSeqTilt.StopPlay
            TiltDelay = 0
            TiltWarning = 0
            If EE Then
                EndEE
            Else
                BallSaved = 0
                AutoPlungerReady = 0
                MultiBallMode = 0
                NextBallDelay = 40
            End If
        End If
        Exit Sub
    End If

    'only for this table

    If EE Then
        If BallSaved Then
            StartScarShooting 1
        Else
            If BallsOnPlayfield = 0 Then
                EndEE
            End If
        End If
        Exit Sub
    End If

    If EventStarted = 8 Then
        Exit Sub
    End If

    If BallSaved Then
        DD "-", CenterBottom("DONT MOVE"), "", eNone, eBlink, 0, 1000, True, "vo_ballsaved"
        AutoPlungerReady = 1
        If MultiBallMode = 1 Then
            MultiBallDelay = MultiBallDelay + 20 'add a new multiball
        Else
            EndBallSaved
            If EventStarted Then
                NewBall8
            Else
                NewBall
            End If
        End If
        Exit Sub
    End If

    If BallsOnPlayfield = 1 Then 'this is the last multiball so turn off multiballs and other effects
        MultiBallMode = 0
        AutoPlungerReady = 0
        'end multiball modes
        If EventStarted = 11 Then
            EndEvent11
        End If
        EndHitMan
        EndYeyoRun
        EndTWIY
    End If

    If BallsOnPlayfield = 0 Then ' this is the last ball
        If ExtraBallAwarded Then
            DD "", CenterBottom("SAME PLAYER"), "", eNone, eBlink, 0, 500, True, "vo_shootagain"
            DD "", CenterBottom("SHOOT AGAIN"), "", eNone, eBlink, 0, 500, True, ""
            InitExtraBall
            AutoPlungerReady = 0
            NewBall
        Else
            SkillshotReady = 1
            StopSong
            'PlaySound "drainball"
            GiOff
            'FlashEffectOff 0
            DisplayFlushQueue()
            BonusDelay = 40
            'end modes
            'EndSideMode will be executed through each side end event
            EndMainEvent
            CloseLockBlock
            EndGetCar
            EndBank
        End If
    End If
End Sub

'******************
' New Ball Release
'******************

Sub NextBall
    Player = Player + 1 'otherwise move to the next player
    If Player> Players Then
        BallsRemaining = BallsRemaining - 1
        Player = 1
    End If

    If BallsRemaining = 0 Then
        StopSong
        AllLampsOff
        MatchDelay = 100
        Exit Sub
    End If

    ' GiEffect1
    GiOn
    If SkillshotReady Then
        StartSkillshot
    End If
    'DisplayScoreNow
    If Balls = 1 Then Say "Free_advice_dont_fuck_with_me", "14242424247313132222722424":If B2SOn Then Controller.B2SStartAnimation("Tony")
    If Balls = 2 Then Say "I_feel_like_I_already_know_you", "1324171223122413":If B2SOn Then Controller.B2SStartAnimation("Tony")
    If Balls >= 3 Then Say "Watch_yourself_man", "1324238523133435212314241":If B2SOn Then Controller.B2SStartAnimation("Tony")
    NewBall
End Sub

Sub NewBall
    If(BallsOnPlayfield + BallsinLock) = MaxBalls Then Exit Sub 'Exit sub if total balls on table is maxed out
    BallRelease.createball
    BallsOnPlayfield = BallsOnPlayfield + 1
    BallRelease.Kick 90, 8
    PlaySoundat SoundFXDOF("fx_ballrel",108, DOFPulse,DOFContactors), Ballrelease
    If SkillshotReady Then
        PlaySong "bgout_scarface_NOE.mp3"
    Else
        PlayTheme
    End If
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

'***********************
'       BALL SAVE
'***********************

' lights: l1 (this is the Ball Save light)

Dim BallSavedDelay

Sub InitBallSaver
    BallSavedDelay = 0
    BallSaved = 0
    l1.State = 0
End Sub

Sub StartBallSaved(seconds)
    If BallSavedDelay <20 * seconds Then
        BallSavedDelay = BallSavedDelay + 20 * seconds ' times 20 because of the game timer being 50 units, 1000 = 1 sec
    End If
    BallSaved = 1
    l1.BlinkInterval = 100
    l1.State = 2
End Sub

Sub BlinkFastBallSaved
    l1.State = 0
    l1.BlinkInterval = 20
    l1.State = 2
End Sub

Sub EndBallSaved
    'AutoPlungerReady = 0
    If MultiBallmode = 1 Then 'turn off the ballsaver timer during multiball when the timer is 0
        If BallSavedDelay = 0 Then
            AutoPlungerReady = 0
            BallSaved = 0
            l1.State = 0
        End If
    Else
        BallSavedDelay = 0
        BallSaved = 0
        l1.State = 0
    End If
End Sub

'************
' Extra ball
'************
' only one extra ball per player
'light 49

Sub GiveExtraBall
    ExtraBallAwarded = 1
    ExtraBallIsLit = 0
    l49.State = 0
    l1.State = 1
    PlaySound SoundFXDOF("fx_knocker", 134, DOFPulse, DOFKnocker)
    DD "", "", "dmdextraball1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdextraball2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdextraball3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdextraball4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdextraball5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdextraball4", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdextraball5", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdextraball4", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdextraball5", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdextraball4", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdextraball5", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdextraball4", eNone, eNone, 0, 150, TRUE, ""
    Say "Want_to_get_a_drink", "3425233424261":If B2SOn Then Controller.B2SStartAnimation("Tony")
End Sub

Sub InitExtraBall
    ExtraBallAwarded = 0
    ExtraBallIsLit = 0
    l1.State = 0
    l49.State = 0
End Sub

Sub LitExtraBall
    If ExtraBallAwarded Then Exit Sub 'do not lit the extraball light if already awarded since only one extra ball per ball
    DD "-", "EXTRABALL IS LIT", "", eNone, eBlinkFast, 0, 1000, True, "vo_extraballislit"
    ExtraBallIsLit = 1
    l49.State = 2
    OnlyOpenLockBlock
End Sub

'************
' Special
'************

Sub GiveSpecial
    SpecialAwarded = 1
    SpecialIsLit = 0
    l55.State = 0
    PlaySound SoundFXDOF("fx_knocker", 134, DOFPulse, DOFKnocker)
    Credits = Credits + 1:DisplayCredits
	If Credits = 1 then
	    DOF 132, DOFOn
	End If
    DD "-", CenterBottom("SPECIAL"), "", eNone, eBlinkFast, 0, 1000, True, "vo_special"
End Sub

Sub LitSpecial
    DD "-", CenterBottom("SPECIAL IS LIT"), "", eNone, eBlinkFast, 0, 1000, True, "vo_specialislit"
    SpecialIsLit = 1
    l55.State = 2
End Sub

Sub InitSpecial
    SpecialAwarded = 0
    SpecialIsLit = 0
    l55.State = 0
End Sub

'***************
' Add Multiballs
'***************
' Used to add mutiballs to the table

Sub AddMultiballs(nr)
    MultiBallMode = 1
    MultiballDelay = MultiballDelay + nr * 20
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal keycode)
    If Gamestarted AND NOT Tilted AND EventStarted <> 8 Then
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1
        Else
            If keycode = LeftFlipperKey Then PlaySound "WilliamsBong":TempEgg = TempEgg + "l":CheckStartEgg
            If keycode = RightFlipperKey Then PlaySound "WilliamsBong":TempEgg = TempEgg + "r":CheckStartEgg
    End If

    If keycode = AddCreditKey Then
        If credits = 0 Then Say "Ok_lets_make_this_happen", "12361214231":If B2SOn Then Controller.B2SStartAnimation("Tony")
        credits = credits + 1
		If credits = 1 Then
			DOF 132, DOFOn	
		End If
        PlaySound "fx_Coin"
        If credits> 15 Then credits = 15
        DisplayCredits
        savehs()
    End If

    If keycode = AddCreditKey2 Then
        If credits = 0 Then Say "Ok_lets_make_this_happen", "12361214231":If B2SOn Then Controller.B2SStartAnimation("Tony")
        credits = credits + 5
		If credits = 5 Then
		    DOF 132, DOFOn
		End If
        PlaySound "fx_Coin"
        If credits> 15 Then credits = 15
        DisplayCredits
        savehs()
    End If

    If keycode = StartGameKey Then
        If credits> 0 Then

' B2S turn off gameover 2-10-13
	If B2SOn Then 
		Controller.B2SSetGameOver 0
	End If
            If players <4 Then
                If GameStarted = 0 Then
                    Players = Players + 1
                    Player = 1
                    credits = credits - 1
					If credits = 0 Then 
						DOF 132, DOFOff	
					End If
                    DisplayFlushQueue()
                    DD CenterTop("CREDITS " &Credits), CenterBottom(Players & " PLAYERS"), "", eNone, eNone, 0, 1000, True, ""
                    savehs()
                    If players = 1 AND BallsOnPlayfield = 0 Then StartGame
                    Else
                        If Player = 1 AND BallsRemaining = BallsPerGame Then
                            Players = Players + 1
                            credits = credits - 1
							If credits = 0 Then 
								DOF 132, DOFOff	
							End If
                            DisplayFlushQueue()
                            DD CenterTop("CREDITS " &Credits), CenterBottom(Players & " PLAYERS"), "", eNone, eNone, 0, 1000, True, ""
                            savehs()
                        End If
                End If
            End If
        Else
            SaySomethingWhileWaiting
            DisplayFlushQueue()

            DD CenterTop("CREDITS " &Credits), CenterBottom("INSERT COIN"), "", eNone, eBlink, 0, 1000, FALSE, ""
            If GameStarted = 0 Then ShowTableInfo
        End If
    End If
 
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25: Bump
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25: Bump
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25: Bump
    If keycode = PlungerKey Then :Plunger.Pullback::AutoPlungerReady = 0
    If hsbModeActive Then HighScoreProcessKey(keycode)
    If keycode = 35 Then ResetHighscore 'H key
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If keycode = PlungerKey Then
        Plunger.Fire
        If(BallinPlunger = 1) then 'the ball is in the plunger lane
            PlaySoundat "fx_Plunger2", plunger
        else
            PlaySoundat "fx_Plunger", plunger
        end if
    End If
    If GameStarted = 1 AND NOT Tilted AND EventStarted <> 8 Then
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If
End Sub

'********************
' Special JP Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundat SoundFXDOF("fx_flipperup", 101, DOFOn, DOFContactors), leftflipper
		LeftFlipper.EOSTorque = 0.85:LeftFlipper.RotateToEnd
        RotateLightsLeft
    Else
		PlaySoundat SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFContactors), leftflipper
		LeftFlipper.EOSTorque = 0.2:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
		PlaySoundat SoundFXDOF("fx_flipperup", 102, DOFOn, DOFContactors), rightflipper
		RightFlipper.EOSTorque = 0.85:RightFlipper.RotateToEnd
        RotateLightsRight
    Else
		PlaySoundat SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFContactors), rightflipper
		RightFlipper.EOSTorque = 0.2:RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
PlaySound "fx_rubber_flipper", 0, parm/60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
PlaySound "fx_rubber_flipper", 0, parm/60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub


'*******************************
'    JP's Alpha Ramp Plunger
'     for non-vpm tables
'*******************************

Dim BallinPlunger

Sub swPlunger_Hit 'plunger lane switch
    NextBallBusy = 1
    BallinPlunger = 1
    LastSwitchHit = "swPlunger"
    If EventStarted = 8 Then AutoPlungerReady = 0
    If AutoPlungerReady Then AutoPlungerDelay = 10
End Sub

Sub swPlunger_UnHit
    BallinPlunger = 0
    AutoPlungerDelay = 0
    AutoPlungerReady = 0
    NextBallBusy = 0
End Sub


'Autoplunger

Sub AutoPlungerFire:PlaySoundat SoundFXDOF("fx_popper", 123, DOFPulse, DOFContactors), plunger:AutoPlunger.Fire:AutoPlunger.TimerEnabled = 1:End Sub
Sub AutoPlunger_Timer:AutoPlunger.PullBack:AutoPlunger.TimerEnabled = 0:End Sub

'*****************
'      Tilt
'*****************

Sub Bump()
Dim i
    If GameStarted AND Tilted = 0 AND Tilt <8 Then
        Tilt = Tilt + 1
        If Tilt = 3 AND TiltWarning = 0 Then
            TiltWarning = 1
            Say "You_fuckin_high_or_what", "131426141":If B2SOn Then Controller.B2SStartAnimation("Tony")
            DD "", "", "dmdtilt1", eNone, eNone, 0, 600, FALSE, ""
            DD "", "", "dmdtilt2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdtilt3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdtilt4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdtilt5", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt5", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt5", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt5", eNone, eNone, 0, 200, True, ""
        End If
        If Tilt = 5 AND TiltWarning = 1 Then
            TiltWarning = 2
            Say "Fuckin_cock_sucker", "8524252524":If B2SOn Then Controller.B2SStartAnimation("Tony")
            DD "", "", "dmdtilt1", eNone, eNone, 0, 600, FALSE, ""
            DD "", "", "dmdtilt2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdtilt3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdtilt4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdtilt5", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt5", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt5", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdtilt5", eNone, eNone, 0, 200, True, ""
        End If
        If Tilt> 7 Then
            DisplayFlushQueue()
            Say "heyfuckyouman", "162527171":If B2SOn Then Controller.B2SStartAnimation("Tony")
            DD "", "", "dmdtilt6", eNone, eNone, 0, 600, FALSE, ""
            DD "", "", "dmdtilt7", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdtilt8", eNone, eNone, 0, 900, FALSE, ""
            DD "", "", "dmdtilt9", eNone, eNone, 0, 3000, FALSE, ""
            Tilted = True
            for i = 210 to 224
                DOF i, DOFOff
            next
            GiOff
            LightSeqTilt.Play SeqAllOff
            TiltObjects 1
        End If

        TiltDelay = 20
    End If
End Sub

Sub TiltObjects(Enabled)
    If Enabled Then
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Force = 1
        Bumper2.Force = 1
        Bumper3.Force = 1
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        Bumper1.Force = 5
        Bumper2.Force = 5
        Bumper3.Force = 5
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

'**********
' Game Over
'**********

Sub GameOver

'B2S gameover 2-10-13 added
'Random ending line
Dim wiseguy  
 DOF 210, DOFOff
 Wiseguy = CInt(Int((10 * Rnd()) + 1))
        Select Case wiseguy
            Case 1:Say "saygoodnight", "251414152424231":If B2SOn Then Controller.B2SStartAnimation("Tony") '2-10-13 added
            Case 2:Say "I_tell_you_something_fuck_you_man", "232414239535251":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 3:Say "I_told_you_not_to_fuck_with_me", "2335145336543332151":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 4:Say "Now_your_fucked", "26361":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 5:Say "Ok_your_starting_to_piss_me_off", "222514122315421":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 6:Say "Run_while_you_can_stupid_fuck", "13131335142121431":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 7:Say "You_boring_you_know_that", "153634151":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 8:Say "You_just_fucked_up_man", "24251423341":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 9:Say "You_need_an_army_to_take_me", "13151414141413331":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 10:Say "You_picked_the_wrong_guy_to_fuck_with", "23232523233334261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        End Select 
    If B2SOn Then Controller.B2SSetGameOver 1
    Dim tmp
    tmp = Score(1)
    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

    If tmp> HighScore(1) Then 'add 1 credit for beating the highscore
        credits = credits + 1
		If Credits = 1 then
		   DOF 132, DOFOn
		End If
    End If

    If tmp> HighScore(3) Then
        PlaySound SoundFXDOF("fx_knocker", 134, DOFPulse, DOFKnocker)
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        GameOver_Part2
    End If

End Sub

Sub GameOver_Part2
    Savehs
    GameStarted = 0
    AllLampsOff
    GiOff
    LeftFlipper.RotateToStart
    RightFlipper.RotateToStart
    AttractMode_On
    Players = 0
    Player = 0
    StopSong
End Sub

'************************
' Play Musics MP3 version
'************************

Dim SongPlaying

Sub PlaySong(sng)
    If MusicOn Then
        If SongPlaying <> sng Then
            PlayMusic sng
            SongPlaying = sng
        End If
    End If
End Sub

Sub table1_MusicEnded 'repeats the same song again
    SongPlaying = ""
    PlayTheme
End Sub

Sub StopSong 'stop current song
    SongPlaying = ""
    EndMusic
End Sub

Sub PlayTheme 'select a song depending of the mode
    If SideEventStarted Then
        Select Case SideEventNr
            Case 1:PlaySong "bgout_scarface_SEHU.mp3"
            Case 2:PlaySong "bgout_scarface_SEH.mp3"
            Case 3:PlaySong "bgout_scarface_MB.mp3"
            Case 4:PlaySong "bgout_scarface_SEY.mp3"
            Case 5:PlaySong "bgout_scarface_SEHU.mp3"
        End Select

        Exit Sub
    End If

    If EventStarted Then
        Select Case EventStarted
            Case 1:PlaySong "bgout_scarface_ME1.mp3"
            Case 2:PlaySong "bgout_scarface_ME2.mp3"
            Case 3:PlaySong "bgout_scarface_ME3.mp3"
            Case 4:PlaySong "bgout_scarface_ME4.mp3"
            Case 5:PlaySong "bgout_scarface_ME5.mp3"
            Case 6:PlaySong "bgout_scarface_ME6.mp3"
            Case 7:PlaySong "bgout_scarface_ME7.mp3"
            Case 8:PlaySong "bgout_scarface_ME8.mp3"
            Case 9:PlaySong "bgout_scarface_ME9.mp3"
            Case 10:PlaySong "bgout_scarface_ME10.mp3"
            Case 11:PlaySong "bgout_scarface_ME11.mp3"
        End Select
    Else
        PlaySong "bgout_scarface_NOE.mp3"
    End If

End Sub


'***********************
' Ramp Helpers & Others
'***********************

Sub RHelp1_Hit()
    StopSound "Wire Ramp"

End Sub

Sub RHelp2_Hit()
    StopSound "Wire Ramp"

End Sub

Sub RHelp3_Hit()
    RotRounds = 0
    TurnTableOn
    StopSound "Wire Ramp"

    SetLamp 5, 1, 2, 4
    LockPost.IsDropped = 0:DOF 135, DOFPulse
    If YeyoStarted Then
        StartRightRampJackpot
    End If
End Sub


'*****************
' Display Credits
'*****************

Sub DisplayCredits
    DisplayFlushQueue()
    DD CenterTop("CREDITS " &Credits), "-", "", eNone, eNone, 0, 500, True, ""
    If GameStarted = 0 Then ShowTableInfo
End Sub

'*****************
' Scores AND Bonus
'*****************

Dim Score(4), Bonus, BonusCount, BonusMultiplier, BonusHeld
Dim HighScore(4), HighScoreName(4)
Dim JackpotValue, Goal1, Goal2, Made1(4), Made2(4)

Sub AddScore(sumtoadd)
    If NOT Tilted Then
        Score(player) = Score(player) + sumtoadd ' * (xMultiplier) * SpecialMultiplier
        If EndWinEvent11Delay Then Exit Sub       'do not display the score while animating the end.
        If EventStarted Then
            UpdateEventsDMD
        Else
            DisplayScore
        End If
        ' Extra Ball check
        If Score(player)> Goal1 Then
            If Made1(player) = False Then:Made1(player) = True:GiveExtraBall():End If
        End If
        If Score(player)> Goal2 Then
            If Made2(player) = False Then Made2(player) = True:GiveExtraBall():End If
        End If
    End If
End Sub

Sub ResetHighscore
    HighScore(0) = 6000000
    HighScoreName(0) = "HAS"
    HighScore(1) = 5500000
    HighScoreName(1) = "JPS"
    HighScore(2) = 4000000
    HighScoreName(2) = "DWS"
    HighScore(3) = 3500000
    HighScoreName(3) = "SOF"
    JackpotValue = 3000000
    savehs
End Sub

Sub AddBonus(sumtoadd)
    If NOT Tilted Then
        Bonus = Bonus + sumtoadd
        DisplayBottom 0
    End If
End Sub

Sub AddBonusMultiplier 'Increment the BonusMultiplier by 1
    If NOT Tilted Then
        BonusMultiplier = BonusMultiplier + 1
        DisplayBottom 0
    End If
End Sub

'************
' GI Effects
'************

Sub GiOn
	dim xx
	For each xx in GI:xx.State = 1: Next
    PlaySound "fx_relay"
	DOF 138, DOFOn
End Sub

Sub GiOff
	dim xx
	For each xx in GI:xx.State = 0: Next
    PlaySound "fx_relay"
	DOF 138, DOFOff
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue("scarface", "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 6000000 End If

    x = LoadValue("scarface", "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "HAS" End If

    x = LoadValue("scarface", "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 5500000 End If

    x = LoadValue("scarface", "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "JPS" End If

    x = LoadValue("scarface", "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 5000000 End If

    x = LoadValue("scarface", "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "DWS" End If

    x = LoadValue("scarface", "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 4500000 End If

    x = LoadValue("scarface", "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "SOF" End If

    x = LoadValue("scarface", "credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If
End Sub

Sub Savehs
    SaveValue "scarface", "HighScore1", HighScore(0)
    SaveValue "scarface", "HighScore1Name", HighScoreName(0)
    SaveValue "scarface", "HighScore2", HighScore(1)
    SaveValue "scarface", "HighScore2Name", HighScoreName(1)
    SaveValue "scarface", "HighScore3", HighScore(2)
    SaveValue "scarface", "HighScore3Name", HighScoreName(2)
    SaveValue "scarface", "HighScore4", HighScore(3)
    SaveValue "scarface", "HighScore4Name", HighScoreName(3)
    SaveValue "scarface", "Credits", Credits
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub HighScoreEntryInit()
    hsbModeActive = True
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ'<>*+-/=\^0123456789`" ' ` is back arrow
    hsCurrentLetter = 1
    DisplayFlushQueue()
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreProcessKey(keycode)
    If keycode = LeftFlipperKey Then
        playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "`") then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayNameNow()
        end if
    end if
End Sub

Sub HighScoreDisplayNameNow()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayName()
    Dim i
    Dim TempTopStr
    Dim TempBotStr

    TempTopStr = "ENTER YOUR NAME "
    DispCurrentTopLine = TempTopStr
    DMDTop TempTopStr

    TempBotStr = "    > "
    if(hsCurrentDigit> 0) then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    DispCurrentBottomLine = TempBotStr
    DMDBottom TempBotStr
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2) then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "JPS"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    GameOver_Part2
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) <HighScore(j + 1) Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub

' *********************************************************************
'      Easy Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and fast blink
' JP: added support for DMD images and animations
' *********************************************************************

Const dcTopCHARSPERLINE = 10
Const dcBottomCHARSPERLINE = 16

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Dim DispQueueSize
Dim DispQueueHead
Dim DispQueueTail
Dim DisplayTopBlankLine
Dim DisplayBottomBlankLine
Dim DispCurrentTopLine
Dim DispCurrentBottomLine
Dim DispCurrentBottomLine2
Dim DispSwapBottomLine
Dim DispEffectCountT1
Dim DispEffectCountT1End
Dim DispEffectBlinkCycleT1
Dim DispEffectCountT2
Dim DispEffectCountT2End
Dim DispEffectCountT2Dly
Dim DispEffectSpeed
Dim DispEffectBlinkSlowRate
Dim DispEffectBlinkFastRate
Dim DispEffectBlinkCycleT2
Dim DispQueueText1(64)
Dim DispQueueText2(64)
Dim DispQueueText3(64)
Dim DispQueueEffectOnT1(64)
Dim DispQueueEffectOnT2(64)
Dim DispQueueEffectOnT2Dly(64)
Dim DispQueueTimeOn(64)
Dim DispQueuebFlush(64)
Dim DispQueueSound(64)

Sub DisplayInit(EffectSpeed, SlowBlinkRate, QuickBlinkRate)
    Dim i
    DispQueueSize = 64
    DisplayFlushQueue()
    DispEffectSpeed = EffectSpeed
    DispEffectBlinkSlowRate = SlowBlinkRate
    DispEffectBlinkFastRate = QuickBlinkRate
    DisplayTopBlankLine = Space(dcTopCHARSPERLINE)
    DisplayBottomBlankLine = Space(dcBottomCHARSPERLINE)
    DispCurrentTopLine = DisplayTopBlankLine
    DispCurrentBottomLine = DisplayBottomBlankLine
    DispSwapBottomLine = 0
    DMDTop DispCurrentTopLine
    DMDBottom DispCurrentBottomLine
End Sub

Sub DisplayFlushQueue()
    DisplayTimer.Enabled = False
    DisplayEffectTimer.Enabled = False
    DispQueueHead = 0
    DispQueueTail = 0
    DispEffectCountT1 = 0
    DispEffectCountT1End = 0
    DispEffectBlinkCycleT1 = 0
    DispEffectCountT2 = 0
    DispEffectCountT2End = 0
    DispEffectCountT2Dly = 0
    DispEffectBlinkCycleT2 = 0
    ActivateAnimationFrame 0
End Sub

Sub DisplayTop(EffectOnT1) 'only updates the top line
    DD CenterTop(FormatScore(Score(player) ) ), "-", "", EffectOnT1, eNone, 0, 25, True, ""
End Sub

Sub DisplayBottom(EffectOnT1) 'In this table is only used to display the bonus hits
    DisplayFlushQueue
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom(FormatScore(Bonus) & " X" &BonusMultiplier), "", eNone, EffectOnT1, 0, 150, True, ""
End Sub

Function DisplayGetQueueSize()
    DisplayGetQueueSize = DispQueueTail - DispQueueHead
End Function

Sub DD(Text1, Text2, Text3, EffectOnT1, EffectOnT2, DelayT2, TimeOn, bFlush, Sound)
    if(DispQueueTail <DispQueueSize) Then
        if(Text1 = "-") Then EffectOnT1 = eNone
        if(Text2 = "-") Then EffectOnT2 = eNone
        Text1 = Ucase(Text1)
        Text2 = Ucase(Text2)
        DispQueueText1(DispQueueTail) = Text1
        DispQueueText2(DispQueueTail) = Text2
        DispQueueText3(DispQueueTail) = Text3
        DispQueueEffectOnT1(DispQueueTail) = EffectOnT1
        DispQueueEffectOnT2(DispQueueTail) = EffectOnT2
        DispQueueEffectOnT2Dly(DispQueueTail) = DelayT2
        DispQueueTimeOn(DispQueueTail) = TimeOn
        DispQueuebFlush(DispQueueTail) = bFlush
        DispQueueSound(DispQueueTail) = Sound
        DispQueueTail = DispQueueTail + 1
        if(DispQueueTail = 1) Then
            DisplayHead()
        End If
    End If
End Sub

Sub DisplayHead()
    DispEffectCountT1 = 0
    DispEffectCountT2 = 0
    DisplayEffectTimer.Interval = DispEffectSpeed
    DispEffectCountT2Dly = DispQueueEffectOnT2Dly(DispQueueHead)

    Select Case(DispQueueEffectOnT1(DispQueueHead) )
        Case eNone:DispEffectCountT1End = 1
        Case eScrollLeft:DispEffectCountT1End = Len(DispQueueText1(DispQueueHead) )
        Case eScrollRight:DispEffectCountT1End = Len(DispQueueText1(DispQueueHead) )
        Case eBlink:DispEffectCountT1End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
            DispEffectBlinkCycleT1 = 0
        Case eBlinkFast:DispEffectCountT1End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
            DispEffectBlinkCycleT1 = 0
    End Select

    Select Case(DispQueueEffectOnT2(DispQueueHead) )
        Case eNone:DispEffectCountT2End = 1
        Case eScrollLeft:DispEffectCountT2End = Len(DispQueueText2(DispQueueHead) )
        Case eScrollRight:DispEffectCountT2End = Len(DispQueueText2(DispQueueHead) )
        Case eBlink:DispEffectCountT2End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
            DispEffectBlinkCycleT2 = 0
        Case eBlinkFast:DispEffectCountT2End = int(DispQueueTimeOn(DispQueueHead) / DispEffectSpeed)
            DispEffectBlinkCycleT2 = 0
    End Select

    If DispQueueText3(DispQueueHead) <> "" Then
        ActivateAnimationFrame 1
        DMDAnim DispQueueText3(DispQueueHead)
    Else
        ActivateAnimationFrame 0
    End If

    if(DispQueueSound(DispQueueHead) <> "") Then
        PlaySound(DispQueueSound(DispQueueHead) )
    End If
    DisplayEffectTimer.Enabled = True
End Sub

Sub DisplayScoreNow()
    DisplayFlushQueue()
    DisplayScore()
End Sub

Sub DisplayScore()
    Dim tmp
    if(DispQueueHead = DispQueueTail) Then
        DispCurrentTopLine = CenterTop(FormatScore(Score(player) ) )
        DMDTop DispCurrentTopLine
        DispCurrentBottomLine = "PLAYER " & Player & "  BALL " &Chr(Balls + 48)
        DMDBottom DispCurrentBottomLine
    End If
End Sub

Sub DisplayEffectTimer_Timer()
    DisplayEffectTimer.Enabled = False
    DisplayProcessEffectOn()
End Sub

Sub DisplayTimer_Timer()
	' B2S *******************************************************************************
	If B2SCurrentFrame <> "" and B2SOn Then
		Controller.B2SSetData B2SCurrentFrame, 0
		B2SCurrentFrame = ""
	End If
	' B2S *******************************************************************************
    Dim Head
    DisplayTimer.Enabled = False
    Head = DispQueueHead
    DispQueueHead = DispQueueHead + 1
    if(DispQueueHead = DispQueueTail) Then
        if(DispQueuebFlush(Head) = True) Then
            DisplayFlushQueue()
            DisplayScore()
        Else
            DispQueueHead = 0
            DisplayHead()
        End If
    Else
        DisplayHead()
    End If
End Sub

Sub DisplayProcessEffectOn()
    Dim i
    Dim BlinkEffect
    Dim TempTopStr
    Dim TempBottomStr
    Dim TempLeftStr
    Dim TempRightStr
    Dim MaskCharacter

    BlinkEffect = False

    TempLeftStr = ""
    TempRightStr = ""

    if(DispEffectCountT1 <> DispEffectCountT1End) Then
        DispEffectCountT1 = DispEffectCountT1 + 1

        select case(DispQueueEffectOnT1(DispQueueHead) )
            case eNone:
                TempTopStr = DispQueueText1(DispQueueHead)
            case eScrollLeft:
                TempTopStr = Right(DispCurrentTopLine, dcTopCHARSPERLINE - 1)
                TempTopStr = TempTopStr & Mid(DispQueueText1(DispQueueHead), DispEffectCountT1, 1)
            case eScrollRight:
                TempTopStr = Mid(DispQueueText1(DispQueueHead), (dcTopCHARSPERLINE + 1) - DispEffectCountT1, 1)
                TempTopStr = TempTopStr & Left(DispCurrentTopLine, dcTopCHARSPERLINE - 1)
            case eBlink:
                BlinkEffect = True
                if((DispEffectCountT1 MOD DispEffectBlinkSlowRate) = 0) Then
                    DispEffectBlinkCycleT1 = DispEffectBlinkCycleT1 xor 1
                End If

                if(DispEffectBlinkCycleT1 = 0) Then
                    TempTopStr = DispQueueText1(DispQueueHead)
                Else
                    TempTopStr = DisplayTopBlankLine
                End If
            case eBlinkFast:
                BlinkEffect = True
                if((DispEffectCountT1 MOD DispEffectBlinkFastRate) = 0) Then
                    DispEffectBlinkCycleT1 = DispEffectBlinkCycleT1 xor 1
                End If

                if(DispEffectBlinkCycleT1 = 0) Then
                    TempTopStr = DispQueueText1(DispQueueHead)
                Else
                    TempTopStr = DisplayTopBlankLine
                End If
        End Select

        if(DispQueueText1(DispQueueHead) <> "-") Then
            if(Len(TempTopStr)> dcTopCHARSPERLINE) Then
                TempTopStr = Left(TempTopStr, dcTopCHARSPERLINE)
            Else
                if(Len(TempTopStr) <dcTopCHARSPERLINE) Then
                    TempTopStr = TempTopStr & Space(dcTopCHARSPERLINE-Len(TempTopStr) )
                End If
            End If
            DispCurrentTopLine = TempTopStr
            DMDTop DispCurrentTopLine
        End If
    End If

    TempLeftStr = ""
    TempRightStr = ""

    if(DispEffectCountT2 <> DispEffectCountT2End) Then
        if(DispEffectCountT2Dly = 0) Then
            DispEffectCountT2 = DispEffectCountT2 + 1
            select case(DispQueueEffectOnT2(DispQueueHead) )
                case eNone:
                    TempBottomStr = DispQueueText2(DispQueueHead)
                case eScrollLeft:
                    TempBottomStr = Right(DispCurrentBottomLine, dcBottomCHARSPERLINE - 1)
                    TempBottomStr = TempBottomStr & Mid(DispQueueText2(DispQueueHead), DispEffectCountT2, 1)
                case eScrollRight:
                    TempBottomStr = Mid(DispQueueText2(DispQueueHead), (dcBottomCHARSPERLINE + 1) - DispEffectCountT2, 1)
                    TempBottomStr = TempBottomStr & Left(DispCurrentBottomLine, dcBottomCHARSPERLINE - 1)
                case eBlink:
                    BlinkEffect = True
                    if((DispEffectCountT2 MOD DispEffectBlinkSlowRate) = 0) Then
                        DispEffectBlinkCycleT2 = DispEffectBlinkCycleT2 xor 1
                    End If

                    if(DispEffectBlinkCycleT2 = 0) Then
                        TempBottomStr = DispQueueText2(DispQueueHead)
                    Else
                        TempBottomStr = DisplayTopBlankLine
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((DispEffectCountT2 MOD DispEffectBlinkFastRate) = 0) Then
                        DispEffectBlinkCycleT2 = DispEffectBlinkCycleT2 xor 1
                    End If

                    if(DispEffectBlinkCycleT2 = 0) Then
                        TempBottomStr = DispQueueText2(DispQueueHead)
                    Else
                        TempBottomStr = DisplayBottomBlankLine
                    End If
            End Select
            if(DispQueueText2(DispQueueHead) <> "-") Then
                if(Len(TempBottomStr)> dcBottomCHARSPERLINE) Then TempBottomStr = Left(TempBottomStr, dcBottomCHARSPERLINE)
                if(Len(TempBottomStr) <dcBottomCHARSPERLINE) Then TempBottomStr = TempBottomStr & Space(dcBottomCHARSPERLINE-Len(TempBottomStr) )
                DispCurrentBottomLine = TempBottomStr
                DMDBottom DispCurrentBottomLine
            End If
        Else
            DispEffectCountT2Dly = DispEffectCountT2Dly - 1
        End If
    End If

    if(DispEffectCountT1 = DispEffectCountT1End) and(DispEffectCountT2 = DispEffectCountT2End) Then

        if(DispQueueTimeOn(DispQueueHead) = 0) Then
            DisplayFlushQueue()
        Else
            if(BlinkEffect = True) Then
                DisplayTimer.Interval = 10
            Else
                DisplayTimer.Interval = DispQueueTimeOn(DispQueueHead)
            End If

            DisplayTimer.Enabled = True
        End If
    Else
        DisplayEffectTimer.Enabled = True
    End If
End Sub

Function FormatScore(ByVal Num) 'it returns a string with commas as defined in Black's original font
    dim i
    dim NumString

    NumString = CStr(abs(Num) )

    For i = Len(NumString) -3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1) ) then
            NumString = left(NumString, i-1) &chr(asc(mid(NumString, i, 1) ) + 144) &right(NumString, Len(NumString) - i)
        end if
    Next
    FormatScore = NumString
End function

Function CenterTop(NumString)
    Dim tmp
    tmp = (dcTopCHARSPERLINE - Len(NumString) ) \ 2
    CenterTop = Space(tmp) & NumString & Space(tmp)
End Function

Function CenterBottom(NumString)
    Dim tmp
    tmp = (dcBottomCHARSPERLINE - Len(NumString) ) \ 2
    CenterBottom = Space(tmp) & NumString & Space(tmp)
End Function

'***********
' JP's DMD
'***********
Dim Digits, Chars(255)

Sub DMDInit


	If B2SOn Then
	Controller.B2SSetLED 5, 63
	Controller.B2SSetLED 25, 63
	End If

    Dim i
    Digits = Array(digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9, digit10, digit11, _
        digit12, digit13, digit14, digit15, digit16, digit17, digit18, digit19, digit20, digit21, digit22, digit23, digit24, digit25)

    For i = 0 to 255:Chars(i) = "dempty":Next

    '    Chars(34) = '"
    '    Chars(36) = '$
    '    Chars(39) = ''
    '    Chars(42) = '*
    '    Chars(43) = '+
    '    Chars(45) = '-
    '    Chars(47) = '/
    Chars(48) = "d0" '0
    Chars(49) = "d1" '1
    Chars(50) = "d2" '2
    Chars(51) = "d3" '3
    Chars(52) = "d4" '4
    Chars(53) = "d5" '5
    Chars(54) = "d6" '6
    Chars(55) = "d7" '7
    Chars(56) = "d8" '8
    Chars(57) = "d9" '9
    '   Chars(60) = '<
    '   Chars(61) = '=
    '   Chars(62) = '>
    '   Chars(64) = '@
    Chars(65) = "da" 'A
    Chars(66) = "db" 'B
    Chars(67) = "dc" 'C
    Chars(68) = "dd" 'D
    Chars(69) = "de" 'E
    Chars(70) = "df" 'F
    Chars(71) = "dg" 'G
    Chars(72) = "dh" 'H
    Chars(73) = "di" 'I
    Chars(74) = "dj" 'J
    Chars(75) = "dk" 'K
    Chars(76) = "dl" 'L
    Chars(77) = "dm" 'M
    Chars(78) = "dn" 'N
    Chars(79) = "do" 'O
    Chars(80) = "dp" 'P
    Chars(81) = "dq" 'Q
    Chars(82) = "dr" 'R
    Chars(83) = "ds" 'S
    Chars(84) = "dt" 'T
    Chars(85) = "du" 'U
    Chars(86) = "dv" 'V
    Chars(87) = "dw" 'W
    Chars(88) = "dx" 'X
    Chars(89) = "dy" 'Y
    Chars(90) = "dz" 'Z
    '    Chars(92) = '\
    '    Chars(94) = '^
    '    Chars(95) = '_
    '    Chars(96) = '`
    Chars(192) = "d0a" '0.
    Chars(193) = "d1a" '1.
    Chars(194) = "d2a" '2.
    Chars(195) = "d3a" '3.
    Chars(196) = "d4a" '4.
    Chars(197) = "d5a" '5.
    Chars(198) = "d6a" '6.
    Chars(199) = "d7a" '7.
    Chars(200) = "d8a" '8.
    Chars(201) = "d9a" '9

    DisplayInit 20, 5, 2
End Sub

Sub DMDBottom(astr)
	'TextBox2.text = astr
    Dim digit
    For digit = 0 to 15
        DMDDisplayChar mid(astr, digit + 1, 1), digit
		If B2SOn Then
			B2SDMDDisplayChar mid(astr, digit + 1, 1), digit
		End If
    Next

    dmdR.State = ABS(dmdR.State -1)
End Sub

Sub DMDTop(astr)
	'TextBox1.text = astr
    Dim digit
    For digit = 16 to 25
        DMDDisplayChar mid(astr, digit-15, 1), digit
If B2SOn Then
	B2SDMDDisplayChar mid(astr, digit - 16 + 1, 1), digit
End If
    Next

    dmdR.State = ABS(dmdR.State -1)
End Sub

Sub DMDDisplayChar(achar, adigit)
    Dim i
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
End Sub

''''''

Sub B2SDMDDisplayChar(achar, adigit)
dim ledvalue
ledvalue = 0
If achar = "" Then achar = " "
achar = ASC(achar)
Select Case achar
' 0
Case 48: ledvalue = 1+2+4+8+16+32
Case 49: ledvalue = 2+4+1024
Case 50: ledvalue = 1+2+8+16+64+2048
Case 51: ledvalue = 1+2+4+8+2048
Case 52: ledvalue = 2+4+32+64+2048
' 5
Case 53: ledvalue = 1+4+8+32+64+2048
Case 54: ledvalue = 1+4+8+16+32+64+2048
Case 55: ledvalue = 1+2+4
Case 56: ledvalue = 1+2+4+8+16+32+64+2048
Case 57: ledvalue = 1+2+4+8+32+64+2048
' A
Case 65: ledvalue = 1+2+4+16+32+64+2048
Case 66: ledvalue = 1+2+4+8+512+2048+8192
Case 67: ledvalue = 1+8+16+32
Case 68: ledvalue = 1+2+4+8+512+8192
Case 69: ledvalue = 1+8+16+32+64
' F
Case 70: ledvalue = 1+16+32+64
Case 71: ledvalue = 1+4+8+16+32+2048
Case 72: ledvalue = 2+4+16+32+64+2048
Case 73: ledvalue = 1+8+512+8192
Case 74: ledvalue = 2+4+8+16
' K
Case 75: ledvalue = 16+32+64+1024+4096
Case 76: ledvalue = 8+16+32
Case 77: ledvalue = 2+4+16+32+256+1024
Case 78: ledvalue = 2+4+16+32+256+4096
Case 79: ledvalue = 1+2+4+8+16+32
' P
Case 80: ledvalue = 1+2+16+32+64+2048
Case 81: ledvalue = 1+2+4+8+16+32+4096
Case 82: ledvalue = 1+2+16+32+64+2048+4096
Case 83: ledvalue = 1+4+8+32+64+2048
Case 84: ledvalue = 1+512+8192
' U
Case 85: ledvalue = 2+4+8+16+32
Case 86: ledvalue = 16+32+1024+16384
Case 87: ledvalue = 2+4+16+32+4096+16384
Case 88: ledvalue = 256+1024+4096+16384
Case 89: ledvalue = 256+1024+8192
' Z
Case 90: ledvalue = 1+8+1024+16384
' 0.
Case 192: ledvalue = 1+2+4+8+16+32+128
Case 193: ledvalue = 2+4+1024+128
Case 194: ledvalue = 1+2+8+16+64+2048+128
Case 195: ledvalue = 1+2+4+8+2048+128
Case 196: ledvalue = 2+4+32+64+2048+128
' 5.
Case 197: ledvalue = 1+4+8+32+64+2048+128
Case 198: ledvalue = 1+4+8+16+32+64+2048+128
Case 199: ledvalue = 1+2+4+128
Case 200: ledvalue = 1+2+4+8+16+32+64+2048+128
Case 201: ledvalue = 1+2+4+8+32+64+2048+128
'
Case Else: ledvalue = 0
End Select
If B2SOn Then Controller.B2SSetLED adigit+1, ledvalue
End Sub
' B2S *******************************************************************************

''''''
Sub ActivateAnimationFrame(Enabled) 'switch the normal score display and teh animation display
    Dim i
    For i = 0 to 25
        Digits(i).visible = Not Enabled
    Next

    digit26.visible = Enabled

	If B2SOn Then
		If Enabled Then
			Controller.B2SHideScoreDisplays
		Else
			If B2SCurrentFrame <> "" Then
				Controller.B2SSetData B2SCurrentFrame, 0
				B2SCurrentFrame = ""
			End If
		Controller.B2SShowScoreDisplays
		End If
	End If
End Sub

Sub DMDAnim(frame)
    digit26.ImageA = frame
    dmdR.State = ABS(dmdR.State -1)

	If B2SOn Then
		B2SCurrentFrame = frame
		Controller.B2SSetData frame, 1
	End If
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
	PlaySong "bgout_scarface_ME1.mp3"			  '2-10-13 added    
    'DD CenterTop("GAME OVER"), " ", "", eBlink, eNone, 0, 2000, False, ""
    DD "", "", "gameover", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "gameover", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "gameover", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "gameover", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "", eNone, eNone, 0, 400, FALSE, ""
    If Credits> 0 Then
        DD CenterTop("CREDITS " &Credits), CenterBottom("PRESS START"), "", eNone, eBlink, 0, 2000, FALSE, ""
    Else
        DD CenterTop("CREDITS " &Credits), CenterBottom("INSERT COIN"), "", eNone, eBlink, 0, 2000, FALSE, ""
    End If
    DD CenterTop("JPSALAS"), CenterBottom("AND"), "", eNone, eNone, 0, 2000, False, ""
    DD CenterTop("hassanchop"), CenterBottom("presents"), "", eNone, eNone, 0, 2000, False, ""
    DD "", "", "DMD AM1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM3", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM5", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM6", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM7", eNone, eNone, 0, 2000, FALSE, ""
    DD "", "", "DMD AM8", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM11", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM11", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM11", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM11", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "DMD AM13", eNone, eNone, 0, 2000, FALSE, ""
    DD CenterTop("SCARFACE"), CenterBottom("VERSION " &Version), "", eNone, eNone, 0, 2500, False, ""
    DD SPACE(dcTopCHARSPERLINE), SPACE(dcBottomCHARSPERLINE), "", eScrollLeft, eScrollLeft, 6, 20, False, ""
    DD CenterTop("HIGHSCORES"), SPACE(dcBottomCHARSPERLINE), "", eScrollLeft, eScrollLeft, 0, 20, FALSE, ""
    ' DD CenterTop("HIGHSCORES"), "-", eBlinkFast, eNone, 0, 1000, FALSE, ""
    DD "-", Left("1 " &HighScoreName(0) & " " &FormatScore(HighScore(0) ) & "      ", dcBottomCHARSPERLINE), "", eNone, eScrollLeft, 6, 2000, FALSE, ""
    DD "-", Left("2 " &HighScoreName(1) & " " &FormatScore(HighScore(1) ) & "       ", dcBottomCHARSPERLINE), "", eNone, eScrollLeft, 6, 2000, FALSE, ""
    DD "-", Left("3 " &HighScoreName(0) & " " &FormatScore(HighScore(2) ) & "      ", dcBottomCHARSPERLINE), "", eNone, eScrollLeft, 6, 2000, FALSE, ""
    DD "-", Left("4 " &HighScoreName(1) & " " &FormatScore(HighScore(3) ) & "       ", dcBottomCHARSPERLINE), "", eNone, eScrollLeft, 6, 2000, FALSE, ""
    DD "-", SPACE(dcBottomCHARSPERLINE), "", eNone, eScrollLeft, 6, 20, FALSE, ""
    DD Right(Space(dcTopCHARSPERLINE) & FormatScore(Score(player) ), dcBottomCHARSPERLINE), " ", "", eNone, eNone, 0, 2000, FALSE, ""
    DD SPACE(dcTopCHARSPERLINE), SPACE(dcBottomCHARSPERLINE), "", eScrollLeft, eScrollLeft, 0, 500, FALSE, ""
End Sub

Sub AttractMode_On()
	If B2SOn Then
		Controller.B2SStartAnimation("Title") 
		Controller.B2SStartAnimation("Startup") 
		Controller.B2SStartAnimation("Muzzle") 	
	End If
	PlaySong "bgout_scarface_ME1.mp3"			  '2-10-13 added    
    SetupAttractMode()
    PlaySong "bgout_scarface_NOE.mp3"
    DisplayFlushQueue
    ShowTableInfo
End Sub

Sub AttractMode_Off()
    LightSeqAttract.StopPlay
End Sub

Sub SetupAttractMode()
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    SetupAttractMode()
End Sub

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqEE_PlayDone()
    EasterLights
End Sub


'*******************************************************************
'           >>>        Here starts game code     <<<
'*******************************************************************
' Any target hit sub will follow this:
' - play a sound
' - do some physical movement
' - add a score
' - check some variables/modes this trigger is a member of
' - set the "LastSwicthHit" variable
'*******************************************************************

'Variables

Dim ScoreSkillShot
Dim ScoreBumpers
Dim ScoreSuperBumpers
Dim ScoreCar
Dim ScoreOuterLoop
Dim ScoreRightRamp
Dim ScoreMiniLoop
Dim ScoreCenterHole
Dim ScoreLeftRamp
Dim ScoreBankLights
Dim ScoreDrops
Dim ScoreTWIYJackpot
Dim ScoreLock
Dim ScoreSpinnerTargets
Dim ScoreCollectYeyo
Dim ScoreME1Complete
Dim ScoreME2Part
Dim ScoreME2Complete
Dim ScoreME3Part
Dim ScoreME3Complete
Dim ScoreME4Part
Dim ScoreME4Complete
Dim ScoreME5Part
Dim ScoreME5Complete
Dim ScoreME6Part
Dim ScoreME6Complete
Dim ScoreME7Part
Dim ScoreME7Complete
Dim ScoreME8Skill
Dim ScoreME8Miss
Dim ScoreME9PArt
Dim ScoreME9Complete
Dim ScoreME10Part
Dim ScoreME10Complete
Dim ScoreME11Jackpot
Dim ScoreME11SuperJackpot
Dim ScoreCompleteLanes
Dim ScoreHitmanJackpot
Dim ScoreHitmanJackpotIncrease
Dim ScoreCarJackpot
Dim ScoreBankRunJackpot
Dim ScoreSlingshots
Dim ScoreYeyoJackpot

Sub InitVariables
    'score variables
    ScoreSkillShot = 25000
    ScoreBumpers = 100
    ScoreSuperBumpers = 1 'this is a multiplier
    ScoreCar = 500
    ScoreOuterLoop = 1000
    ScoreRightRamp = 5000
    ScoreMiniLoop = 150
    ScoreCenterHole = 100
    ScoreLeftRamp = 2000
    ScoreBankLights = 500
    ScoreDrops = 30
    ScoreTWIYJackpot = 500000
    ScoreLock = 2000
    ScoreSpinnerTargets = 100
    ScoreCollectYeyo = 2000
    ScoreME1Complete = 200000
    ScoreME2Part = 2500
    ScoreME2Complete = 200000
    ScoreME3Part = 2500
    ScoreME3Complete = 400000
    ScoreME4Part = 2500
    ScoreME4Complete = 400000
    ScoreME5Part = 2500
    ScoreME5Complete = 500000
    ScoreME6Part = 2500
    ScoreME6Complete = 400000
    ScoreME7Part = 2500
    ScoreME7Complete = 800000
    ScoreME8Skill = 20000
    ScoreME8Miss = 0
    ScoreME9PArt = 2500
    ScoreME9Complete = 400000
    ScoreME10Part = 50000
    ScoreME10Complete = 1000000
    ScoreME11Jackpot = 200000
    ScoreME11SuperJackpot = 10000000
    ScoreCompleteLanes = 60
    ScoreHitmanJackpot = 300000
    ScoreHitmanJackpotIncrease = 10000
    ScoreCarJackpot = 500000
    ScoreBankRunJackpot = 500000
    ScoreSlingshots = 10
    ScoreYeyoJackpot = 500000

    ' other variables
    BumperHits = 0
    VukDelay = 0
    EjectBallsInDeskDelay = 0
End Sub

'************************
'       Lock hole
'************************
' kicker: lock
' lights 50, 51, 52

Dim EjectXLockedBallDelay, OpenLockBlockDelay, StartTWIYDelay

Sub OpenLockBlock
    If EventStarted = 11 Then Exit Sub
    DD "", "", "dmdlocklit", eNone, eNone, 0, 2000, TRUE, ""
    PlaySound SoundFX("fx_solenoidon", DOFContactors)
    LockBlock.IsDropped = 1
    lockblock1.IsDropped = 0
    BallLockEnabled = 1
    UpdateLockLights
End Sub

Sub OnlyOpenLockBlock
    If EventStarted = 11 Then Exit Sub
    PlaySound SoundFX("fx_solenoidon", DOFContactors)
    LockBlock.IsDropped = 1
    lockblock1.IsDropped = 0
End Sub

Sub CloseLockBlock
    PlaySound SoundFX("fx_solenoidoff", DOFContactors)
    LockBlock.IsDropped = 0
    lockblock1.IsDropped = 1
End Sub

Sub InitLock
    l50.State = 0
    l51.State = 0
    l52.State = 0
    BallsInLock = 0
    LockBlock.IsDropped = 0
    LockBlock1.IsDropped = 1
    BallLockEnabled = 0
    OpenLockBlockDelay = 0
End Sub

Sub Lock_Hit
    PlaySoundat "fx_hole_enter",lock
    
    Lock.Destroyball
    BallsInLock = BallsInLock + 1
    BallsOnPlayfield = BallsOnPlayfield - 1
    If ExtraBallIsLit Then
        GiveExtraBall
    End If
    If BallLockEnabled = 0 Then 'not in lock mode so eject back the ball and close the gate
        PlaySoundat "fx_solenoidoff",lock
        LockBlock.IsDropped = 0
        lockblock1.IsDropped = 1
        EjectXLockedBallDelay = 40
        Exit Sub
    End If
    If BallLockEnabled AND BallsInLock = 1 Then '1st ball locked
        Say "Oh_baby", "44241":If B2SOn Then Controller.B2SStartAnimation("Tony")
        UpdateLockLights
        DD "", "", "dmdlock1", eNone, eNone, 0, 400, FALSE, ""
        DD "", "", "dmdlock2", eNone, eNone, 0, 400, FALSE, ""
        DD "", "", "dmdlock3", eNone, eNone, 0, 1500, True, ""
        AddMultiballs 1                         'add one ball to the plunger
    End If

    If BallLockEnabled AND BallsInLock = 2 Then '2nd ball locked
        Say "Dont_make_me_spank_you", "3323242413631313121313":If B2SOn Then Controller.B2SStartAnimation("Tony")
        UpdateLockLights
        DD "", "", "dmdlock4", eNone, eNone, 0, 400, FALSE, ""
        DD "", "", "dmdlock5", eNone, eNone, 0, 400, FALSE, ""
        DD "", "", "dmdlock6", eNone, eNone, 0, 1500, True, ""
        AddMultiballs 1                         'add one ball to the plunger
    End If

    If BallLockEnabled AND BallsInLock = 3 Then '3rd ball locked
        Say "Im_Tony_fuckin_M", "13362526251":If B2SOn Then Controller.B2SStartAnimation("Tony")
        UpdateLockLights
        DD "", "", "dmdlock6", eNone, eNone, 0, 400, FALSE, ""
        DD "", "", "dmdlock7", eNone, eNone, 0, 400, FALSE, ""
        DD "", "", "dmdlock8", eNone, eNone, 0, 800, FALSE, ""
        DD "", "", "dmdlock9", eNone, eNone, 0, 200, FALSE, ""
        DD "", "", "dmdlock10", eNone, eNone, 0, 200, FALSE, ""
        DD "", "", "dmdlock9", eNone, eNone, 0, 200, FALSE, ""
        DD "", "", "dmdlock10", eNone, eNone, 0, 200, FALSE, ""
        DD "", "", "dmdlock9", eNone, eNone, 0, 200, FALSE, ""
        DD "", "", "dmdlock10", eNone, eNone, 0, 2000, TRUE, ""
        BallLockEnabled = 0:CloseLockBlock	
        StartTWIYDelay = 100
    End If
End Sub

Sub UpdateLockLights
    Select Case BallsInLock
        Case 0:l50.State = 2
        Case 1:l50.State = 1:l51.State = 2
        Case 2:l50.State = 1:l51.State = 1:l52.State = 2
        Case 3:l50.State = 1:l51.State = 1:l52.State = 1
    End Select
End Sub

Sub EjectBallsLocked 'all the locked balls
    If BallsInLock> 0 Then EjectLockedBallsDelay = 40
End Sub

Sub LockExit            'from the front of the desk
    Dropall             'in case they are up
    ResetDropDelay = 20 'reset them right after
    Deskin.createball
    Deskin.Kick 188 + RND(3) * 3, 20 + RND(1) * 3
End Sub

'******************
' Helpers & effects
'******************

Dim LaneGateDelay

Sub LaneGateT_Hit
    LaneBlock.IsDropped = 1
	DOF 125, DOFPulse
    LaneGateDelay = 6
End Sub

Sub LaneGateClose
    LaneBlock.IsDropped = 0
End Sub

'*************
' Holes & Vuks
'*************

Dim PlungerLaneKDelay, BallsInPlungerLane
Dim CarHoleDelay

Sub plungerlanein_Hit
    If EventStarted = 8 Then
        
        BallsOnPlayfield = BallsOnPlayfield - 1
        plungerlanein.destroyball
        PlaySoundat "fx_kicker_enter", plungerlanein
        NewBall8
        Exit Sub
    End If

    If SkillshotReady Then
        StartBallSaved(BallSavedTime) 'start ball save time if 1st ball
    End If
    PlaySoundat "fx_kicker_enter", plungerlanein
    If SkillshotStarted Then PlaySound "car_not_start":EndSkillshot
    BallsInPlungerLane = BallsInPlungerLane + 1
    
    plungerlanein.destroyball
    PlungerLaneKDelay = 20
End Sub

Sub PlungerLaneExit
    If ExtraBallislit OR BallLockEnabled Then 'close the diverter
        CloseLockBlock
        OpenLockBlockDelay = 20
    End If
    plungerlaneout.createball
    plungerlaneout.kick 0, 26 + Int(RND(1) * 8)
    PlaySoundat SoundFXDOF("fx_kicker", 124, DOFPulse, DOFContactors), plungerlaneout
End Sub

Sub Vukin_Hit
    PlaySoundat "fx_kicker_enter", vukin
    VukDelay = 30
End Sub

Sub VukEject
    vukin.destroyball
    vukout.createball
    vukout.kick 90, 24, 1.28
    PlaySoundat SoundFXDOF("fx_popper", 118, DOFPulse, DOFContactors), vukout
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	
	PlaySoundat SoundFXDOF("right_slingshot", 104, DOFPulse, DOFContactors) , sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1

    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    Addscore ScoreSlingshots
    LastSwitchHit = "rightslingshot"
    'modes
    LightNextEvent

End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySoundat SoundFXDOF("left_slingshot", 103, DOFPulse, DOFContactors), sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1

    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    Addscore ScoreSlingshots
    LastSwitchHit = "leftslingshot"
    'modes
    LightNextEvent

End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

'**********
' Bumpers
'**********

Dim BumperHits, bumperoffdelay
 
Sub Bumper1_Hit
    PlaySoundat SoundFXDOF("fx_bumper", 105, DOFPulse, DOFContactors), bumper1: Bumper1.TimerEnabled = 1:DOF 214, DOFPulse
    'score & bonus
    'checkmodes this switch is part of	
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 5 Then
        PlaySound "drums1"
        CheckEvent5Bumps
    Else
        SetLamp 2, 1, 0, 0
        BumperHits = BumperHits + 1
        AddScore ScoreBumpers * ScoreSuperBumpers
        CheckSuperPops
    End If
    LastSwitchHit = "bumper1"
End Sub

Sub Bumper1_Timer()
	SetLamp 2, 0, 0, 0
	l48.State = 0:
	Bumper1.TimerEnabled = 0
End Sub

Sub Bumper2_Hit
    PlaySoundat SoundFXDOF("fx_bumper", 107, DOFPulse, DOFContactors), bumper2:Bumper2.TimerEnabled = 1:DOF 214, DOFPulse
    'score & bonus
    'checkmodes this switch is part of	
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 5 Then
        PlaySound "drums2"
        CheckEvent5Bumps
    Else
        SetLamp 2, 1, 0, 0
        BumperHits = BumperHits + 1
        AddScore ScoreBumpers * ScoreSuperBumpers
        CheckSuperPops
    End If
    LastSwitchHit = "bumper2"
End Sub

Sub Bumper2_Timer()
	SetLamp 2, 0, 0, 0
	l48.State = 0:
	Bumper1.TimerEnabled = 0
End Sub
 
Sub Bumper3_Hit
    PlaySoundat SoundFXDOF("fx_bumper", 106, DOFPulse, DOFContactors), bumper3:Bumper3.TimerEnabled = 1:DOF 214, DOFPulse
    'score & bonus
    'checkmodes this switch is part of	
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 5 Then
        PlaySound "drums2"
        CheckEvent5Bumps
    Else
        SetLamp 2, 1, 0, 0
        BumperHits = BumperHits + 1
        AddScore ScoreBumpers * ScoreSuperBumpers
        CheckSuperPops
    End If
    LastSwitchHit = "bumper3"
End Sub

Sub Bumper3_Timer()
	SetLamp 2, 0, 0, 0
	l48.State = 0:
	Bumper1.TimerEnabled = 0
End Sub

Sub CheckSuperPops
    Dim tmp
    DisplayFlushQueue
    If BumperHits <100 Then
        DD "-", CenterBottom("BUMPER HITS " &BumperHits), "", eNone, eNone, 0, 200, True, ""
        tmp = INT(RND(1) * 3)
        Select case tmp
            Case 0:PlaySound "punch1"
            Case 1:PlaySound "punch1"
            Case 2:PlaySound "punch1"
        End Select
    Else
        DD "-", CenterBottom("SUPER BUMPER HIT"), "", eNone, eNone, 0, 200, True, "snap1"
    End If

    If BumperHits = 100 Then
        LightBumer1.State = 2
        LightBumer2.State = 2
        LightBumer3.State = 2
        LightBumer1a.State = 2
        LightBumer2a.State = 2
        LightBumer3a.State = 2
        ScoreSuperBumpers = 100  ' multiplier
        bumperoffdelay = 60 * 20 '20 because the gametimer is 50. 1000 is 1 second
    End If
End Sub

Sub InitBumpers
        LightBumer1.State = 1
        LightBumer2.State = 1
        LightBumer3.State = 1
        LightBumer1a.State = 1
        LightBumer2a.State = 1
        LightBumer3a.State = 1
    ScoreSuperBumpers = 1 ' multiplier
    bumperoffdelay = 0
End Sub

'***********
' Rollovers
'***********

Sub lat1_Hit
	PlaySoundat "fx_sensor", lat1
    DOF 128, DOFOn
    'score & bonus
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If l7.State = 1 Then
        PlaySound "scpoing4"
    Else
        PlaySound "reload4"
    End If
    AddScore 10 '???
    l7.State = 1
    'checkmodes this switch is part of
    LastSwitchHit = "lat1"
    CheckFlipperLights
End Sub
 
Sub lat1_UnHit
	DOF 128, DOFOff
End Sub
 
Sub lat2_Hit
	PlaySoundat "fx_sensor", lat2
    DOF 129, DOFOn 
    'score & bonus
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If l7.State = 1 Then
        PlaySound "scpoing4"
    Else
        PlaySound "reload4"
    End If
    AddScore 10 '???
    l8.State = 1
    'checkmodes this switch is part of
    LastSwitchHit = "lat2"
    CheckFlipperLights
End Sub
 
Sub lat2_UnHit
	DOF 129, DOFOff
End Sub
 
Sub lat3_Hit
	PlaySoundat "fx_sensor", lat3
    DOF 130, DOFOn
    'score & bonus
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If l7.State = 1 Then
        PlaySound "scpoing4"
    Else
        PlaySound "reload4"
    End If
    AddScore 10 '???
    l9.State = 1
    'checkmodes this switch is part of
    LastSwitchHit = "lat3"
    CheckFlipperLights
End Sub
 
Sub lat3_UnHit
	DOF 130, DOFOff
End Sub
 
Sub lat4_Hit
	PlaySoundat "fx_sensor", lat4
    DOF 131, DOFOn
    'score & bonus
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If l7.State = 1 Then
        PlaySound "scpoing4"
    Else
        PlaySound "reload4"
    End If
    AddScore 10 '???
    l10.State = 1
    'checkmodes this switch is part of
    LastSwitchHit = "lat4"
    CheckFlipperLights
End Sub
 
Sub lat4_UnHit
	DOF 131, DOFOff
End Sub
 
Sub lat5_Hit
    PlaySoundat "fx_sensor", lat5
	DOF 203, DOFOn
    'score & bonus
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    PlaySound "scpoing10"
    'checkmodes this switch is part of
    If EventStarted = 10 Then
        Event10Hits(1) = 1
        l16.State = 0
        CheckEvent10
    Else
        AddScore 10 '???
    End If
    LastSwitchHit = "lat5"
End Sub
 
Sub lat5_UnHit
	DOF 203, DOFOff
End Sub
 
Sub lat6_Hit
    PlaySoundat "fx_sensor", lat6
	DOF 201, DOFOn
    'score & bonus
    'checkmodes this switch is part of
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    LastSwitchHit = "lat6"
End Sub
 
Sub lat6_UnHit
	DOF 201, DOFOff
End Sub
 
Sub lat7_Hit
    PlaySoundat "fx_sensor" , lat7
	DOF 202, DOFOn
    'score & bonus
    'checkmodes this switch is part of
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    LastSwitchHit = "lat7"
End Sub
 
Sub lat7_UnHit
	DOF 202, DOFOff
End Sub
 
Sub lat8_Hit
    PlaySoundat "fx_sensor", lat8
	DOF 204, DOFOn
    'score & bonus
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    PlaySound "Ba_Dum_Tss1"
    'checkmodes this switch is part of
    If EventStarted = 10 Then
        Event10Hits(2) = 1
        l53.State = 0
        CheckEvent10
    Else
        AddScore 10 '???
    End If
    LastSwitchHit = "lat8"
End Sub
 
Sub lat8_UnHit
	DOF 204, DOFOff
End Sub
 
Sub LeftRampDone_Hit
	If B2SOn Then
		Controller.B2SStartAnimation("PalmTrees")
    End If
	SetLamp 3, 0, 1, 10
    PlaySoundat "Wire Ramp",LeftRampDone:DOF 120, DOFPulse
    'score & bonus
    'checkmodes this switch is part of
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 10 Then
        AddScore ScoreLeftRamp
        Event10Hits(3) = 1
        l23.State = 0
        CheckEvent10
        Exit Sub
    End If
    If EventStarted = 6 AND Event6Drop Then
        AddScore ScoreLeftRamp
        WinEvent6
        Exit Sub
    End If
    If EventStarted = 1 Then
        AddScore ScoreLeftRamp
        WinEvent1
        Exit Sub
    End If
    If TWIYStarted AND LeftRampJackpotEnabled Then
        If RightRampJackpotEnabled = 0 Then
            l25.State = 2
            l42.State = 2
        End If
        StopLeftRampJackpot
        GiveTWIYJackpot
        Exit Sub
    End If
    If HitManStarted Then
        AddScore ScoreHitmanJackpot
        DisplayFlushQueue
        PlaySound "chananJPk"
        DD "", "", "dmdhitman1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdhitman2", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdhitman3", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdhitman4", eNone, eNone, 0, 400, FALSE, ""
        DD CenterTop("JACKPOT "), CenterBottom(FormatScore(ScoreHitmanJackpot) ), "", eNone, eNone, 0, 1000, True, ""
    Else
        AddScore ScoreLeftRamp
        PlaySound "gun2"
        KillRampHits = KillRampHits + 1
        CheckKillRamp
    End if
    LastSwitchHit = "leftrampdone"
End Sub
 
Sub RightRampDone_Hit
    PlaySoundat "Wire Ramp",RightRampDone:DOF 133, DOFPulse
    TurnTableOff 'turn off the rotating platform
    'checkmodes this switch is part of
    LastSwitchHit = "RightRampDone"
End Sub

'***********
' Targets
'***********

Sub rott1_Hit
    PlaySoundatball SoundFX("fx_target",DOFTargets)
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    AddBonus 30
    'checkmodes this switch is part of
    LastSwitchHit = "rott1"
    If EventStarted = 4 Then
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub


Sub rott2_Hit
    PlaySoundatball SoundFX("fx_target",DOFTargets)
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    AddBonus 30
    'checkmodes this switch is part of
    LastSwitchHit = "rott2"
    If EventStarted = 4 Then
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub


Sub rott3_Hit
    PlaySoundatball SoundFXDOF("fx_target", 205, DOFPulse, DOFTargets)
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    AddBonus 30
    'checkmodes this switch is part of
    LastSwitchHit = "rott3"
    If EventStarted = 4 Then
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub



Sub rott4_Hit
	PlaySoundatball SoundFXDOF("fx_target", 206, DOFPulse, DOFTargets)
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    AddBonus 30
    'checkmodes this switch is part of
    LastSwitchHit = "rott4"
    If EventStarted = 4 Then
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub



Sub rott5_Hit
	PlaySoundatball SoundFXDOF("fx_target", 207, DOFPulse, DOFTargets)
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    AddBonus 30
    'checkmodes this switch is part of
    LastSwitchHit = "rott5"
    If EventStarted = 4 Then
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub



Sub rott6_Hit
	PlaySoundatball SoundFXDOF("fx_target", 208, DOFPulse, DOFTargets)
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    AddBonus 30
    'checkmodes this switch is part of
    LastSwitchHit = "rott6"
    If EventStarted = 4 Then
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub



Sub rott7_Hit
	PlaySoundatball SoundFXDOF("fx_target", 209, DOFPulse, DOFTargets)
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    AddScore ScoreSpinnerTargets
    PlaySound "scpoing9"
    AddBonus 30
    'checkmodes this switch is part of
    LastSwitchHit = "rott7"
    If EventStarted = 4 Then
        CheckrottHits
    Else
        CheckYeyo
    End If
End Sub


'*****************************************************************************
'Bank Targets 
'*****************************************************************************

lt1a.Isdropped = 1:lt2a.Isdropped = 1:lt3a.Isdropped = 1:lt4a.Isdropped = 1

Sub lt1_Hit
	PlaySoundat SoundFXDOF("fx_target", 113, DOFPulse, DOFDropTargets), l11
    lt1.isdropped = 1:lt1a.isdropped = 0:lt1.timerenabled = 1
    l11.State = 1
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If BankStarted Then
        DisplayFlushQueue
        LightSeqWinEvent.Play SeqRandom, 60, , 1000
        DD "", "", "dmdbankrun1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun2", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun3", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun4", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun5", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun6", eNone, eNone, 0, 800, TRUE, ""
        AddScore ScoreBankRunJackpot
        EndBank
        Exit Sub
    End If
    If EventStarted = 7 AND Event7State = 0 Then
        CheckEvent7
        AddScore ScoreDrops
        Exit Sub
    End If
    If EventStarted = 10 Then
        Event10Hits(4) = 1
        l11.State = 0
        l12.State = 0
        l13.State = 0
        l14.State = 0
        CheckEvent10
        AddScore ScoreDrops
        Exit Sub
    End If
    AddScore ScoreDrops
    BankTargets(1) = 1
    PlaySound "ker-chin1"
    CheckBank
    'checkmodes this switch is part of
    LastSwitchHit = "lt1"
End Sub

Sub lt1_Timer:lt1.isdropped = 0:lt1a.isdropped = 1:lt1.timerenabled = 0:End Sub

Sub lt2_Hit
	PlaySoundat SoundFXDOF("fx_target", 114, DOFPulse, DOFDropTargets), l12
    lt2.isdropped = 1:lt2a.isdropped = 0:lt2.timerenabled = 1
    l12.State = 1
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If BankStarted Then
        DisplayFlushQueue
        LightSeqWinEvent.Play SeqRandom, 60, , 1000
        DD "", "", "dmdbankrun1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun2", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun3", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun4", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun5", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun6", eNone, eNone, 0, 800, TRUE, ""
        AddScore ScoreBankRunJackpot
        EndBank
        Exit Sub
    End If
    AddScore ScoreDrops
    If EventStarted = 7 AND Event7State = 0 Then
        CheckEvent7
        Exit Sub
    End If
    If EventStarted = 10 Then
        Event10Hits(4) = 1
        l11.State = 0
        l12.State = 0
        l13.State = 0
        l14.State = 0
        CheckEvent10
        Exit Sub
    End If
    BankTargets(2) = 1
    AddScore ScoreDrops
    PlaySound "ker-chin1"
    CheckBank
    'checkmodes this switch is part of
    LastSwitchHit = "lt2"
End Sub

Sub lt2_Timer:lt2.isdropped = 0:lt2a.isdropped = 1:lt2.timerenabled = 0:End Sub

Sub lt3_Hit
	PlaySoundat SoundFXDOF("fx_droptarget", 115, DOFPulse, DOFDropTargets), l13
    lt3.isdropped = 1:lt3a.isdropped = 0:lt3.timerenabled = 1
    l13.State = 1
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If BankStarted Then
        DisplayFlushQueue
        LightSeqWinEvent.Play SeqRandom, 60, , 1000
        DD "", "", "dmdbankrun1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun2", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun3", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun4", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun5", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun6", eNone, eNone, 0, 800, TRUE, ""
        AddScore ScoreBankRunJackpot
        EndBank
        Exit Sub
    End If
    AddScore ScoreDrops
    If EventStarted = 7 AND Event7State = 0 Then
        CheckEvent7
        Exit Sub
    End If
    If EventStarted = 10 Then
        Event10Hits(4) = 1
        l11.State = 0
        l12.State = 0
        l13.State = 0
        l14.State = 0
        CheckEvent10
        Exit Sub
    End If
    BankTargets(3) = 1
    AddScore ScoreDrops
    PlaySound "ker-chin1"
    CheckBank
    'checkmodes this switch is part of
    LastSwitchHit = "lt3"
End Sub

Sub lt3_Timer:lt3.isdropped = 0:lt3a.isdropped = 1:lt3.timerenabled = 0:End Sub

Sub lt4_Hit
	PlaySoundat SoundFXDOF("fx_droptarget", 116, DOFPulse, DOFDropTargets), l14
    lt4.isdropped = 1:lt4a.isdropped = 0:lt4.timerenabled = 1
    l14.State = 1
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If BankStarted Then
        DisplayFlushQueue
        LightSeqWinEvent.Play SeqRandom, 60, , 1000
        DD "", "", "dmdbankrun1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun2", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun3", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun4", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun5", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdbankrun6", eNone, eNone, 0, 800, TRUE, ""
        AddScore ScoreBankRunJackpot
        EndBank
        Exit Sub
    End If
    AddScore ScoreDrops
    If EventStarted = 7 AND Event7State = 0 Then
        CheckEvent7
        Exit Sub
    End If
    If EventStarted = 10 Then
        Event10Hits(4) = 1
        l11.State = 0
        l12.State = 0
        l13.State = 0
        l14.State = 0
        CheckEvent10
        Exit Sub
    End If
    BankTargets(4) = 1
    AddScore ScoreDrops
    PlaySound "ker-chin1"
    CheckBank
    'checkmodes this switch is part of
    LastSwitchHit = "lt4"
End Sub

Sub lt4_Timer:lt4.isdropped = 0:lt4a.isdropped = 1:lt4.timerenabled = 0:End Sub


'*************
' Car Side
'*************


Sub rt1_Hit
    PlaySoundat SoundFX("fx_target",DOFContactors), CarHole
    SetLamp 4, 0, 1, 8
    ' score
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 10 Then
        AddScore ScoreCar
        Event10Hits(5) = 1
        l17.State = 0
        CheckEvent10
        Exit Sub
    End If
    If GetCarStarted Then
        CarHits = 0
        setlamp 1, 0, 1, 10
        AddScore ScoreCarJackpot
        DisplayFlushQueue
        LightSeqWinEvent.Play SeqRandom, 60, , 1000
        DD "", "", "dmdgetcar2", eNone, eNone, 0, 300, FALSE, "car_start"
        DD "", "", "dmdgetcar3", eNone, eNone, 0, 300, FALSE, "car_start"
        DD "", "", "dmdgetcar4", eNone, eNone, 0, 300, FALSE, "car_start"
        DD "", "", "dmdgetcar5", eNone, eNone, 0, 1500, TRUE, ""
        EndGetCar
        Exit Sub
    End If
    'checkmodes this switch is part of
    AddScore ScoreCar
    LastSwitchHit = "rt1"
    CarHits = CarHits + 1
    PlaySound "car_acell":DOF 126, DOFPulse
    CheckCarHits
End Sub

'*****************************************************************************
'Drop Targets  Center
'*****************************************************************************

Dim DroppedTargets(3), CurrentCT, CTDelay
Dim ResetDropDelay

Sub ResetDroptargets
    PlaySoundat SoundFXDOF("fx_resetdrop", 112, DOFPulse, DOFContactors), DeskIn
    ct1.isdropped = 0
    ct2.isdropped = 0
    ct3.isdropped = 0
    DroppedTargets(0) = 0
    DroppedTargets(1) = 0
    DroppedTargets(2) = 0
    DroppedTargets(3) = 0
    Door.Isdropped = 0
    CurrentCT = 0
    If EventStarted = 0 Then
        CTDelay = 91
        l38.State = 0
        l39.State = 0
        l40.State = 0
        l37.State = 0
    End If
    ResetDropDelay = 0
End Sub

Sub DropAll
    PlaySoundat SoundFX("fx_droptarget", DOFcontactors), DeskIn
	DOF 112, DOFPulse
    ct1.isdropped = 1
    ct2.isdropped = 1
    ct3.isdropped = 1
    DroppedTargets(1) = 1
    DroppedTargets(2) = 1
    DroppedTargets(3) = 1
    Door.IsDropped = 1
    CTDelay = 0
    If EventStarted = 0 Then
        l38.State = 1
        l39.State = 1
        l40.State = 1
        l37.State = 2
    End If
End Sub

Sub Door_Hit:PlaySoundat "fx_metalhit", DeskIn:DOF 117, DOFPulse:End Sub

Sub ct1_Hit
    PlaySoundat SoundFXDOF("fx_droptarget", 109, DOFPulse, DOFDropTargets), DeskIn
    ct1.isdropped = 1
    l38.State = 1
    DroppedTargets(1) = 1
    ' score
    'checkmodes this switch is part of
    If EE Then
        PlayRamdomPunch
        ct1.TimerEnabled = 1
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 10 Then
        AddScore ScoreDrops
        ct1.TimerEnabled = 1
        l37.State = 0
        l38.State = 0
        l39.State = 0
        l40.State = 0
        Event10Hits(6) = 1
        CheckEvent10
        Exit Sub
    End If
    If EventStarted = 6 Then
        AddScore ScoreDrops
        CheckEvent6
        Exit Sub
    End If
    If HitManStarted Then
        AddScore ScoreDrops
        ScoreHitmanJackpot = ScoreHitmanJackpot + ScoreHitmanJackpotIncrease
        DD "-", CenterBottom("JACKPOT IS " & FormatScore(ScoreHitmanJackpot) ), "", eNone, eNone, 0, 2000, True, ""
        ct1.TimerEnabled = 1
        Exit Sub
    End If
    If EventStarted = 2 Then
        ct1.TimerEnabled = 1
        l38.State = 2
        AddScore ScoreDrops
        CTHits = CTHits + 1
        If CTHits = 6 Then WinEvent2
        Exit Sub
    End If

    If EventStarted OR SideEventStarted Then 'if any other event is started  then also raise up again the target, but only score points
        ct1.TimerEnabled = 1
        l38.State = 0
        AddScore ScoreDrops
        Exit Sub
    End If

    ' normal working of the droptargets before en event
    If CurrentCT = 1 Then
        CTDelay = 0
        CurrentCT = 0
        DropAll
        AddScore ScoreDrops * 3
    Else
        AddScore ScoreDrops
        PlaySound "gun6"
        If CTDelay> 0 then
            l39.State = 0
            l40.State = 0
            CTDelay = 0
            CurrentCT = 0
        End If
        CheckDropTargets
    End If
End Sub

Sub ct1_Timer
    ct1.TimerEnabled = 0
    PlaySoundat"fx_resetdrop", DeskIn
    ct1.IsDropped = 0
	DroppedTargets(1) = 0
End Sub

Sub ct2_Hit
    PlaySoundat SoundFXDOF("fx_droptarget", 110, DOFPulse, DOFDropTargets), DeskIn
    ct2.isdropped = 1
    l39.State = 1
    DroppedTargets(2) = 1
    ' score
    If EE Then
        PlayRamdomPunch
        ct2.TimerEnabled = 1
        AddScore 1
        Exit Sub
    End If
    'checkmodes this switch is part of
    If EventStarted = 6 Then
        AddScore ScoreDrops
        CheckEvent6
        Exit Sub
    End If
    If EventStarted = 10 Then
        AddScore ScoreDrops
        ct2.TimerEnabled = 1
        l37.State = 0
        l38.State = 0
        l39.State = 0
        l40.State = 0
        Event10Hits(6) = 1
        CheckEvent10
        Exit Sub
    End If
    If HitManStarted Then
        AddScore ScoreDrops
        ScoreHitmanJackpot = ScoreHitmanJackpot + ScoreHitmanJackpotIncrease
        DD "-", CenterBottom("JACKPOT IS " & FormatScore(ScoreHitmanJackpot) ), "", eNone, eNone, 0, 2000, True, ""
        ct2.TimerEnabled = 1
        Exit Sub
    End If
    If EventStarted = 2 Then 'raise up again the target
        ct2.TimerEnabled = 1
        l39.State = 2
        AddScore ScoreDrops
        CTHits = CTHits + 1
        If CTHits = 6 Then WinEvent2
        Exit Sub
    End If

    If EventStarted OR SideEventStarted Then 'if any other event is started  then also raise up again the target, but only score points
        ct2.TimerEnabled = 1
        l39.State = 0
        AddScore ScoreDrops
        Exit Sub
    End If

    ' normal working of the droptargets before en event
    If CurrentCT = 2 Then
        CTDelay = 0
        CurrentCT = 0
        DropAll
        AddScore ScoreDrops * 3
    Else
        AddScore ScoreDrops
        PlaySound "gun6"
        If CTDelay> 0 then
            l38.State = 0
            l40.State = 0
            CTDelay = 0
            CurrentCT = 0
        End If
        CheckDropTargets
    End If
End Sub

Sub ct2_Timer
    ct2.TimerEnabled = 0
    PlaySoundat "fx_resetdrop", DeskIn
    ct2.IsDropped = 0
	DroppedTargets(2) = 0
End Sub

Sub ct3_Hit
    PlaySoundat SoundFXDOF("fx_droptarget", 111, DOFPulse, DOFDropTargets), DeskIn
    ct3.isdropped = 1
    l40.State = 1
    DroppedTargets(3) = 1
    ' score
    If EE Then
        PlayRamdomPunch
        ct3.TimerEnabled = 1
        AddScore 1
        Exit Sub
    End If
    'checkmodes this switch is part of
    If EventStarted = 6 Then
        AddScore ScoreDrops
        CheckEvent6
        Exit Sub
    End If
    If EventStarted = 10 Then
        AddScore ScoreDrops
        ct3.TimerEnabled = 1
        l37.State = 0
        l38.State = 0
        l39.State = 0
        l40.State = 0
        Event10Hits(6) = 1
        CheckEvent10
        Exit Sub
    End If
    If HitManStarted Then
        AddScore ScoreDrops
        ScoreHitmanJackpot = ScoreHitmanJackpot + ScoreHitmanJackpotIncrease
        DD "-", CenterBottom("JACKPOT IS " & FormatScore(ScoreHitmanJackpot) ), "", eNone, eNone, 0, 2000, True, ""
        ct3.TimerEnabled = 1
        Exit Sub
    End If
    If EventStarted = 2 Then 'raise up again the target
        ct3.TimerEnabled = 1
        l40.State = 2
        AddScore ScoreDrops
        CTHits = CTHits + 1
        If CTHits = 6 Then WinEvent2
        Exit Sub
    End If

    If EventStarted OR SideEventStarted Then 'if any other event is started  then also raise up again the target, but only score points
        ct3.TimerEnabled = 1
        l40.State = 0
        AddScore ScoreDrops
        Exit Sub
    End If

    ' normal working of the droptargets before en event
    If CurrentCT = 3 Then
        CTDelay = 0
        CurrentCT = 0
        DropAll
        AddScore ScoreDrops * 3
    Else
        AddScore ScoreDrops
        PlaySound "gun6"
        If CTDelay> 0 then
            l38.State = 0
            l39.State = 0
            CTDelay = 0
            CurrentCT = 0
        End If
        CheckDropTargets
    End If
End Sub

Sub ct3_Timer
    ct3.TimerEnabled = 0
    PlaySoundat "fx_resetdrop", DeskIn
    ct3.IsDropped = 0
	DroppedTargets(3) = 0
End Sub

Sub CheckDropTargets
    DroppedTargets(0) = DroppedTargets(1) + DroppedTargets(2) + DroppedTargets(3)
    If DroppedTargets(0) = 3 Then DropAll
End Sub

'****************************
' Rotate lights with flippers
'****************************
' lights are l7, l8, l9 and l10

Dim LaneLightsTimes, LaneLightsGoal

Sub InitLaneLights
    LaneLightsTimes = 0
    LaneLightsGoal = 10
End Sub

Sub RotateLightsLeft
    Dim tmp
    tmp = l7.state
    l7.state = l8.state
    l8.state = l9.state
    l9.state = l10.state
    l10.state = tmp
End Sub

Sub RotateLightsRight
    Dim tmp
    tmp = l10.state
    l10.state = l9.state
    l9.state = l8.state
    l8.state = l7.state
    l7.state = tmp
End Sub

Sub CheckFlipperLights
    If l7.State = 1 AND l8.State = 1 AND l9.State = 1 AND l10.State = 1 Then
        LightSeqFlipper.Play SeqBlinking, , 5, 40
        PlaySound "reload5"
        l7.state = 0
        l8.state = 0
        l9.state = 0
        l10.state = 0
        LaneLightsTimes = LaneLightsTimes + 1
        If LaneLightsTimes = LaneLightsGoal Then 'turn on Extraball Light
            LaneLightsGoal = 40                  ' you need 40 for the next extra ball
            LitExtraBall
        End If
    End If
End Sub

'************
' Turn Table
'************

Dim RotSpeed, RotDir, RotOn, RotRounds

Sub TurnHelp_Hit
    RotRounds = RotRounds + 1
    If RotOn = 0 Then TurnTableOn
    If RotRounds> 2 Then LockPost.IsDropped = 1:DOF 135, DOFPulse
    TurnHelp.kick 275, 15 + RND(1) * 5
End Sub

Sub TurnHelp1_Hit
    If RotOn = 0 Then
        TurnTableOn
    End If
    TurnHelp1.kick 320, 15 + RND(1) * 5
End Sub

Sub TurnTableON
    RotOn = 1
    SetLamp 5, 0, 2, 0
    RotSpeed = 0
    RotDir = 1
    Rotation.Enabled = 1:DOF 122, DOFOn
End Sub

Sub TurnTableOff
    RotOn = 0
    SetLamp 5, 0, 0, 0
    RotDir = -1
End Sub

Sub Rotation_Timer
    RotSpeed = RotSpeed + RotDir
    If RotSpeed >= 30 Then RotSpeed = 30
    If RotSpeed <= 0 Then
       Rotation.Enabled = 0:DOF 122, DOFOff       
    End If
    RotatingPlatform.RotAndTra2 = RotatingPlatform.RotAndTra2 + RotSpeed
End Sub

'**************
' Head Tracking
'**************

Dim HeadPos
HeadPos = 3:UpdateHead

Sub UpdateHead
    If ShootingBalls then Exit Sub
    Select Case HeadPos
        Case 1:doll.Image = "doll1"
        Case 2:doll.Image = "doll2"
        Case 3:doll.Image = "doll3"
        Case 4:doll.Image = "doll4"
        Case 5:doll.Image = "doll5"
    End Select
End Sub

Sub Head1_Hit:HeadPos = 1:UpdateHead:End Sub
Sub Head2_Hit:HeadPos = 2:UpdateHead:End Sub
Sub Head3_Hit:HeadPos = 3:UpdateHead:End Sub
Sub Head4_Hit:HeadPos = 4:UpdateHead:End Sub
Sub Head5_Hit:HeadPos = 5:UpdateHead:End Sub

'***********************
'Mouth talking animation
'***********************

Dim MouthPos, MouthReps, MouthIntervals, Voice
MouthPos = 0
MouthReps = 0
MouthIntervals = ""
Voice = ""

Sub Say(sound, interval)
    StopSound Voice
    MouthPos = 0
    Voice = sound
    MouthReps = LEN(interval)
    MouthIntervals = interval
    PlaySound Voice
    If ShootingBalls = 0 Then
        Mouth.Enabled = 1
    End If
End Sub

Sub Mouth_Timer
    Dim tmp, tmpinterval
    tmp = HeadPos + MouthPos * 5
    Select Case tmp
        Case 1, 2, 3, 4, 5:UpdateHead:Mouth.Interval = 40 * ABS(MID(MouthIntervals, Len(MouthIntervals) - MouthReps + 1, 1) )
        Case 6:doll.Image = "doll1a":Mouth.Interval = 40 * ABS(MID(MouthIntervals, Len(MouthIntervals) - MouthReps + 1, 1) )
        Case 7:doll.Image = "doll2a":Mouth.Interval = 40 * ABS(MID(MouthIntervals, Len(MouthIntervals) - MouthReps + 1, 1) )
        Case 8:doll.Image = "doll3a":Mouth.Interval = 40 * ABS(MID(MouthIntervals, Len(MouthIntervals) - MouthReps + 1, 1) )
        Case 9:doll.Image = "doll4a":Mouth.Interval = 40 * ABS(MID(MouthIntervals, Len(MouthIntervals) - MouthReps + 1, 1) )
        Case 10:doll.Image = "doll5a":Mouth.Interval = 40 * ABS(MID(MouthIntervals, Len(MouthIntervals) - MouthReps + 1, 1) )
    End Select

    MouthPos = ABS(MouthPos -1)
    MouthReps = MouthReps -1
    If MouthReps <= 0 Then
        Mouth.Enabled = 0
        Updatehead
    End If
End Sub

'*************************
'Say Something stupid test
'*************************
' for example Say "Bring_it_to_me_cockroach", 313211131111
' the higher the number the longer the pause: 1 short delay, 9 long delay
' first number is always with the mouth close,

Sub SaySomethingStupid
    Dim currentsay
    currentsay = INT(81 * RND(1) )
    Select Case currentsay
        Case 0:Say "Boyfriend_fuck_him", "191589159218121813":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 1:Say "Bring_it_to_me_cockroach", "1622111115121":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 2:Say "Bring_my_car_dont_fuck_around", "179517972636663925":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 3:Say "Cmon_bring_your_army", "13144814":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 4:Say "Cmon_come_and_get_me", "12132643233414":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 5:Say "Cmon_pussy_cat", "16272538":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 6:Say "doing_a_great_job", "13252527":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 7:Say "Dont_fuck_with_me", "34433337":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 8:Say "Dont_make_me_spank_you", "3323242413631313121313":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 9:Say "Dont_you_fucken_listen_man", "2313432613131336":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 10:Say "Dont_you_got_something_better_2_do", "122121212121212122123":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 11:Say "Free_advice_dont_fuck_with_me", "14242424247313132222722424":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 12:Say "Fuck_off", "2939":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 13:Say "Fuck_you_chico", "16574539":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 14:Say "Fuckin_cock_sucker", "8524252524":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 15:Say "fuckin_cockroach", "1934242624":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 16:Say "Fuckin_fuck", "29381":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 17:Say "Go_fuck_yourself", "353528":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 18:Say "Have_my_car_set_to_me_ok", "24152414141726":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 19:Say "Hello_pussy_cat", "16242424":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 20:Say "Hello_you_there", "176427":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 21:Say "hello1", "391":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 22:Say "hello2", "281":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 23:Say "hey", "161":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 24:Say "Hey_there_sweet_cheeks", "2414131314":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 25:Say "heyfuckyouman", "162527171":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 26:Say "How_about_we_get_some_ice_cream", "2792131314141413131131":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 27:Say "I_dont_give_a_fuck", "262423141":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 28:Say "I_feel_like_I_already_know_you", "1324171223122413":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 29:Say "I_like_you_your_like_a_tiger", "2713931234131":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 30:Say "I_need_my_fuckin_car_man", "162424161476371":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 31:Say "I_piss_in_your_face", "2333131313131313132":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 32:Say "I_tell_you_something_fuck_you_man", "232414239535251":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 33:Say "I_told_you_not_to_fuck_with_me", "2335145336543332151":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 34:Say "I_want_to_talk_2_George", "16873332362":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 35:Say "Im_fucked", "5417191":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 36:Say "Im_Tony_fuckin_M", "13362526251":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 37:Say "Im_Tony_fuckin_Montana", "13362526251":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 38:Say "Its_obvious_you_got_an_attitude", "131414242224231414141":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 39:Say "My_name_is_Tony_M", "2414133413341":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 40:Say "No_fuckin_way", "1314251":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 41:Say "Now_your_fucked", "26361":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 42:Say "Oh_baby", "44241":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 43:Say "Ok", "32341":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 44:Say "Ok_lets_make_this_happen", "12361214231":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 45:Say "Ok_so_what_you_doing_later", "3223841214131":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 46:Say "Ok_so_why_dont_we_get_together", "322384121413141":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 47:Say "Ok_your_starting_to_piss_me_off", "222514122315421":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 48:Say "Run_while_you_can_stupid_fuck", "13131335142121431":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 49:Say "saygoodnight", "251414152424231":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 50:Say "sayhello2", "1415142926161":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 51:Say "Shit_ass_fuck", "362434":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 52:Say "Thats_ok", "26351":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 53:Say "Tony_would_like_a_word", "131312363628273414141":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 54:Say "Want_to_get_a_drink", "3425233424261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 55:Say "wastemytime", "13121324461":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 56:Say "Watch_yourself_man", "1324238523133435212314241":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 57:Say "What_are_you_going_to_do_for_me", "1312121312131":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 58:Say "Who_you_think_you_fuckin_with", "1324313131313761":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 59:Say "Why_dont_you_get_fucked", "241414251":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 60:Say "WTF_do_you_want", "27391":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 61:Say "Yeah_its_Tony", "3633371":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 62:Say "Yeah1", "271":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 63:Say "Yeah2", "261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 64:Say "You_better_not_be_comunist", "12131313253423231413325272":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 65:Say "You_boring_you_know_that", "153634151":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 66:Say "You_fuckin_high_or_what", "131426141":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 67:Say "You_fuckin_hossa", "13172617171":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 68:Say "You_fuckin_kidding_me_man", "2324133413141":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 69:Say "You_fuckin_prick", "232423441":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 70:Say "You_guys_dont_quit", "1423142324453313332324253453433233343252433341534341":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 71:Say "You_just_fucked_up_man", "24251423341":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 72:Say "You_know_what_you_are_soft", "242673333574324242423241":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 73:Say "You_know_what_you_need", "26279965342515351":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 74:Say "You_know_who_your_fuckin_with", "25162424261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 75:Say "You_need_an_army_to_take_me", "13151414141413331":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 76:Say "You_picked_the_wrong_guy_to_fuck_with", "23232523233334261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 77:Say "You_think_you_can_fuck_with_me", "2414141423261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 78:Say "You_want_me_dont_you", "4415151":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 79:Say "For_fuck_sake", "231323241":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 80:Say "Hey_who_the_fuck_r_u", "232424241":If B2SOn Then Controller.B2SStartAnimation("Tony")

    End Select
End Sub

Sub SaySomethingWhileWaiting
    Dim tmp
    tmp = INT(RND(1) * 6)
    Select Case tmp
        Case 0:Say "Dont_you_got_something_better_2_do", "122121212121212122123":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 1:Say "Fuck_off", "2939":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 2:Say "You_want_me_dont_you", "4415151":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 3:Say "You_better_not_be_comunist", "12131313253423231413325272":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 4:Say "WTF_do_you_want", "27391":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 5:Say "You_guys_dont_quit", "1423142324453313332324253453433233343252433341534341":If B2SOn Then Controller.B2SStartAnimation("Tony")
    End Select
End Sub

'*******************
' Scarface Shooting
'*******************
' animation and shooting av balls
' 1-disable the head triggers and mouth
' 2-start animation
' 3-shoot balls

Dim ShootingBalls, ScarShootPos, ScarShootBall

Sub InitScarface
    ShootingBalls = 0
    ScarShootPos = 0
    ScarShootBall = 0
End Sub

Sub StartScarShooting(nballs) 'it shoot the ball which started the multiball + multiballs
    ScarShootBall = ScarShootBall + nballs
    If ShootingBalls = 0 Then
        ShootingBalls = 1
        Mouth.Enabled = 0
        ScarShootPos = 0
        ScarAnim.Enabled = 1
    End If
End Sub

Sub ScarAnim_Timer
    Select Case ScarShootPos
        Case 0:doll.Image = "doll2"
        Case 1:doll.Image = "doll2"
        Case 2:doll.Image = "dollg1"
        Case 3:doll.Image = "dollg1"
        Case 4:doll.Image = "dollg1"
        Case 5:doll.Image = "dollg2"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls + 1 Then
                cannon1.createball
                cannon1.kick 160, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 6:doll.Image = "dollg3"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls Then
                cannon2.createball
                cannon2.kick 170, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 7:doll.Image = "dollg4"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls Then
                cannon3.createball
                cannon3.kick 178, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 8:doll.Image = "dollg5"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls Then
                cannon4.createball
                cannon4.kick 190, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 9:doll.Image = "dollg6"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls Then
                cannon5.createball
                cannon5.kick 199, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 10:doll.Image = "dollg7"
        Case 11:doll.Image = "dollg7"
        Case 12:doll.Image = "dollg6"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls Then
                cannon5.createball
                cannon5.kick 199, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 13:doll.Image = "dollg5"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls Then
                cannon4.createball
                cannon4.kick 190, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 14:doll.Image = "dollg4"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls Then
                cannon3.createball
                cannon3.kick 178, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 15:doll.Image = "dollg3"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls Then
                cannon2.createball
                cannon2.kick 170, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 16:doll.Image = "dollg2"
            If ScarShootBall AND(BallsOnPlayfield + BallsinLock) <MaxBalls + 1 Then
                cannon1.createball
                cannon1.kick 160, 16 + RND(1) * 3
                If EE AND ScarShootBall = 1 Then PlaySound "sf_hadouken"
                ScarShootBall = ScarShootBall -1
                BallsOnPlayfield = BallsOnPlayfield + 1
                If BallsOnPlayfield> 1 Then MultiBallMode = 1
                PlaySound "machinegun1"
		If B2SOn Then Controller.B2SStartAnimation("Muzzle")
            End IF
        Case 17:doll.Image = "dollg1"
        Case 18:doll.Image = "dollg1"
            If ScarShootBall Then
                ScarShootPos = 4
            End If
        Case 19:doll.Image = "dollg1"
        Case 20:doll.Image = "doll2":ScarAnim.Enabled = 0:ShootingBalls = 0:If B2SOn Then Controller.B2SStopAnimation("Muzzle")
    End Select

    ScarShootPos = ScarShootPos + 1
'    DollR.State = ABS(DollR.State -1)
End Sub

'*************
' TV animation
'*************

Dim TVPos

Sub InitTV
    TVPos = 0
    TV2.IsDropped = 1
    TV3.IsDropped = 1
    TV4.IsDropped = 1
    TV5.IsDropped = 1
    TV6.IsDropped = 1
    TV7.IsDropped = 1
End Sub

Sub TVTimer_Timer
    Select case TVPos
        Case 0:TV8.IsDropped = 1:Tv1.IsDropped = 0
        Case 1:TV1.IsDropped = 1:Tv2.IsDropped = 0
        Case 2:TV2.IsDropped = 1:Tv3.IsDropped = 0
        Case 3:TV3.IsDropped = 1:Tv4.IsDropped = 0
        Case 4:TV4.IsDropped = 1:Tv5.IsDropped = 0
        Case 5:TV5.IsDropped = 1:Tv6.IsDropped = 0
        Case 6:TV6.IsDropped = 1:Tv7.IsDropped = 0
        Case 7:TV7.IsDropped = 1:Tv8.IsDropped = 0
    End Select

    TVPos = TVPos + 1
    If TVPos> 7 Then TVPos = 0
End Sub

'*******************************************************************************
'      <<  MODES  >>
' In any MODE there will be a:
' - list of switches, lights
' - list of variables
' - init sub -init variables and ready to start
' - check sub -check if it has started, and check if it is completed, give award
' - start sub -mostly it setups a variable, mode started, used in the check sub
' - end sub -to turn off the mode. Many modes will end with the ball others will
'            continue until completed or until the end of the game.
'*******************************************************************************

Dim SideEventNr, SideEventStarted, SideEventPrepared, BallsInDesk, EjectBallsInDeskDelay, SideEvents(5), SideEventsFinished, SidesCompleted

Sub InitModes
    Dim i
    SideEventNr = 0 ' contains the side mode nr
    SideEventStarted = 0
    SideEventPrepared = 0
    l24.State = 0
    l41.State = 0
    SideEventsFinished = 0
    SidesCompleted = 0
    For i = 0 To 5
        SideEvents(i) = 0
    Next

    UpdateSideLights
End Sub

Sub EndSideMode
    SideEvents(SideEventNr) = 1
    SideEventsFinished = SideEvents(1) + SideEvents(2) + SideEvents(3) + SideEvents(4) + SideEvents(5)
    If SidesCompleted = 0 Then
        If SideEventsFinished = 5 Then
            SidesCompleted = 1
        End If
    End If
    SideEventPrepared = 0
    l24.State = 0
    l41.State = 0
    SideEventStarted = 0
    BackDoorPost.IsDropped = 1:DOF 136, DOFPulse
    SideEventNr = 0
    UpdateSideLights
    DisplayScoreNow
    PlayTheme
End Sub

Sub PrepareSideMode 'make ready to start sidemode
    SideEventPrepared = 1
    UpdateSideLights
    l24.State = 2
    l41.State = 2
    BackDoorPost.IsDropped = 0:DOF 136, DOFPulse
End Sub

Sub UpdateSideLights
    If SideEvents(1) Then
        l2.State = 1
    Else
        l2.State = 0
    End If

    If SideEvents(2) Then
        l3.State = 1
    Else
        l3.State = 0
    End If

    If SideEvents(3) Then
        l4.State = 1
    Else
        l4.State = 0
    End If

    If SideEvents(4) Then
        l5.State = 1
    Else
        l5.State = 0
    End If

    If SideEvents(5) Then
        l6.State = 1
    Else
        l6.State = 0
    End If

    If SideEventPrepared Then
        Select Case SideEventNr
            Case 1:l2.State = 2
            Case 2:l3.State = 2
            Case 3:l4.State = 2
            Case 4:l5.State = 2
            Case 5:l6.State = 2
        End Select
    End If
End Sub

Sub BackDoorPost_Hit
    PlaySoundat "fx_metalhit", backdoor
End Sub

Sub BackDoor_Hit
    PlaySoundat "fx_hole_enter", backdoor
    BackDoorPost.IsDropped = 1:DOF 136, DOFPulse
    
    BackDoor.DestroyBall
    BallsInDesk = BallsInDesk + 1
    BallsOnPlayfield = BallsOnPlayfield - 1
    l24.State = 0
    l41.State = 0
    If EventStarted = 11 Then
        If Event11Kill Then
            WinEvent11
            Exit Sub
        Else
            Say "Hey_who_the_fuck_r_u", "232424241":If B2SOn Then Controller.B2SStartAnimation("Tony")
            EjectBallsInDeskDelay = 40 'shoot back the ball
            Exit Sub
        End If
    End If
    If SideEventNr AND SideEventStarted = 0 Then 'a side event is prepared to run so start it
        SideEventStarted = 1
        Select Case SideEventNr
            Case 1:StartBank
            Case 2:StartHitMan
            Case 3:StartTWIY 'started from the lock hole not here
            case 4:StartYeyoRun
            case 5:StartGetCar
        End Select
    Else
        Say "Hey_who_the_fuck_r_u", "232424241":If B2SOn Then Controller.B2SStartAnimation("Tony")
        EjectBallsInDeskDelay = 40 'shoot back the ball
    End If
End Sub

Sub DeskIn_Hit
    PlaySoundat "fx_Ballhit", deskin
    
    DeskIn.DestroyBall
    BallsInDesk = BallsInDesk + 1
    BallsOnPlayfield = BallsOnPlayfield - 1
    If EventStarted = 11 Then
        DeskHits = DeskHits + 1
        SetLamp 1, 0, 1, 8
        AddScore ScoreME11Jackpot
        If DeskHits = 10 Then
            Door.IsDropped = 0
            NextPartEvent11
        Else
            StartScarShooting BallsInDesk
            BallsInDesk = 0
        End If
        Exit Sub
    End If
    Door.IsDropped = 0
    If EventsFinished = 10 AND SidesCompleted Then
        StartEvent11Delay = 30
        Exit Sub
    End If
    l38.state = 0
    l39.state = 0
    l40.state = 0
    l37.state = 0
    StartMainEvent 'start the current event
End Sub

'*********************
' Skillshot - Car Hole
'*********************

'switches: carhole
'lights: rflaher

Dim SkillshotStarted, SkillShotReady, CarHole8Delay

Sub InitSkillShot
    CarHole8Delay = 0
    SkillshotStarted = 0
    SkillShotReady = 1
End Sub

Sub StartSkillshot
    SkillshotStarted = 1                           'Started
    LightSeqPFLights.Play SeqAllOff                'turn off playfield lights
    LightSeqSkillshot.Play SeqBlinking, , 5000, 50 
    SetLamp 4, 1, 0, 0
End Sub

Sub EndSkillshot
    If SkillShotStarted Then
        SkillShotReady = 0
        SkillShotStarted = 0
        LightSeqPFLights.StopPlay
        SetLamp 4, 0, 0, 0
        LightSeqSkillshot.StopPlay
    End If
End Sub

Sub CarHole_Hit
    PlaySoundat SoundFXDOF("fx_kicker_enter", 210, DOFPulse, DOFContactors), carhole
    setlamp 1, 0, 1, 10

    If EventStarted = 8 Then
        AddScore ScoreME8Skill
        DisplayFlushQueue
        DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("GET INTO THE CAR"), "", eNone, eBlink, 0, 1500, FALSE, ""
        DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-2", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-3", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-4", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-5", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-6", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-2", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-3", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-4", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-5", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-6", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-2", eNone, eNone, 0, 300, FALSE, ""
        DD "", "", "dmdevent8-3", eNone, eNone, 0, 300, FALSE, ""
        PlaySound "car_start"
        
        Set aBall = ActiveBall
        aZpos = aBall.Z
        CarHole8Delay = 14
        Exit Sub
    End If

    If SkillshotStarted Then
        StartBallSaved(BallSavedTime) 'start ball save time if 1st ball
        AddScore ScoreSkillShot
        DD CenterTop("SKILLSHOT"), CenterBottom(FormatScore(ScoreSkillShot) ), "", eNone, eBlinkFast, 0, 1000, True, "car_start"
    End If
    CarHoleDelay = 20
End Sub

Sub CarHoleExit 'normal skillshot
    EndSkillshot
    PlaySoundat SoundFXDOF("fx_kicker", 119, DOFPulse, DOFContactors), CarHole
    CarHole.Kick 298, 60
End Sub

Sub CarHole8Exit 'exit from the event 8
    CarHole.TimerInterval = 2
    CarHole.TimerEnabled = 1
End Sub

' Car Hole with animation
Dim aBall, aZpos

Sub CarHole_Timer
    aBall.Z = aZpos
    aZpos = aZpos-4
    If aZpos <0 Then
        Me.TimerEnabled = 0
        Me.DestroyBall
        BallsOnPlayfield = BallsOnPlayfield -1
        'PlaySound "fx_ballhit"
        NewBall8
    End If
End Sub

'********************************
' Left Ramp - kill ramp - Hit Man
' 2 ball multiball
'********************************
'triggers: leftramp
'lights 18,19,20,21,22, 23 and 3

Dim KillRampHits, HitmanStarted, LeftRampJackpotEnabled, StartHitManDelay

Sub InitKillRamp
    KillRampHits = 0
    HitmanStarted = 0
    LeftRampJackpotEnabled = 0
	DOF 216, DOFOff
    l18.State = 0
    l19.State = 0
    l20.State = 0
    l21.State = 0
    l22.State = 0
    l23.State = 0
    l3.State = 0
    StartHitManDelay = 0
End Sub

Sub CheckKillRamp
	If B2SOn Then Controller.B2SStopAnimation("Muzzle")
    Select Case KillRampHits
        Case 1:l18.State = 1
        Case 2:l19.State = 1
        Case 3:l20.State = 1
        Case 4:l21.State = 1
        Case Else
            If EventStarted = 11 Then Exit Sub
            l22.State = 1
            l12.State = 1
            'PrepareHitMan
            l3.State = 2
            SideEventNr = 2
            PrepareSideMode
	If B2SOn Then Controller.B2SStopAnimation("Muzzle")
    End Select
End Sub

Sub StartHitMan
    DisplayFlushQueue
    DD CenterTop("HIT MAN"), CenterBottom("STARTING"), "", eNone, eBlinkFast, 0, 1000, FALSE, ""
    DD CenterTop("HIT MAN"), CenterBottom("SHOOT KILL RAMP"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop("HIT MAN"), CenterBottom("FOR JACKPOT"), "", eNone, eBlink, 0, 500, TRUE, ""
    l18.State = 0
    l19.State = 0
    l20.State = 0
    l21.State = 0
    l22.State = 0
    l23.State = 0
    l24.State = 0
    l41.State = 0
    PlayTheme
    ResetDroptargets 'droptargets increase the jackpot during this event
    CTDelay = 0
    l38.State = 1
    l39.State = 1
    l40.State = 1
    StartHitManDelay = 100
End Sub

Sub StartHitMan2
    StartScarShooting BallsInDesk + 1 'shoot the ball/s which started the event + 1 multiball
    BallsinDesk = 0
    StartBallSaved 20                 '20 seconds
    HitmanStarted = 1
    StartLeftRampJackpot              'lights
End Sub

Sub EndHitMan
    If l3.State = 2 Then l3.State = 1
    If HitmanStarted Then 'hitman was started
        KillRampHits = 0
        HitmanStarted = 0
        LeftRampJackpotEnabled = 0
	StopLeftRampJackpot 'Lights
        l3.State = 1        'hitman done
        ResetDroptargets
        EndSideMode
    End If
End Sub

Sub StartLeftRampJackpot
    LeftRampJackpotEnabled = 1
    LightSeqLeftRamp.Play SeqRandom, 10, 10
	If B2SOn Then Controller.B2SStartAnimation("Muzzle")
End Sub

Sub LightSeqLeftRamp_PlayDone()
    LightSeqLeftRamp.Play SeqRandom, 10, 10
End Sub

Sub StopLeftRampJackpot
    LeftRampJackpotEnabled = 0
    LightSeqLeftRamp.StopPlay
	If B2SOn Then Controller.B2SStopAnimation("Muzzle")
End Sub

'********************
' Spinner - sell Yeyo
'********************

'switches : rott1, rott2, rott3, rott4, rott5
'lights 43, 44, 45, 46, 47, 48
'YeyoLights: 0 off, 1 blinking, 2 collected

Dim YeyoHits, YeyoLight, YeyoStarted, RightRampJackpotEnabled

Sub InitYeyo
    YeyoHits = 0
    l43.State = 0
    l44.State = 0
    l45.State = 0
    l46.State = 0
    l47.State = 0
    YeyoLight = 0
    l5.state = 0
    YeyoStarted = 0
    RightRampJackpotEnabled = 0
End Sub

Sub CheckYeyo
    YeyoHits = YeyoHits + 1
    If YeyoStarted AND YeyoHits >= 10 Then '10 hits turn on the jackpot on the right ramp
        YeyoHits = 0
        StartRightRampJackpot
    End If

    If YeyoHits >= 30 Then
        YeyoHits = 0
        YeyoLight = YeyoLight + 1
        Addscore ScoreCollectYeyo
        Select Case YeyoLight
            Case 1:l43.State = 2
            Case 2:l44.State = 2
            Case 3:l45.State = 2
            Case 4:l46.State = 2
            Case 5:l47.State = 2
        End Select
    End If
End Sub

Sub RightRamp_Hit
	If B2SOn Then Controller.B2SStartAnimation("PalmTrees") 
    SetLamp 3, 0, 1, 10
    'AddScore ScoreRightRamp
    PlaySound "money1"
    If EE Then
        PlayRamdomPunch
        AddScore 1
        Exit Sub
    End If
    If EventStarted = 7 Then
        If Event7State = 1 Then
            CheckEvent7
            AddScore ScoreRightRamp
        End If
        Exit Sub
    End If
    If EventStarted = 10 Then
        Event10Hits(7) = 1
        l48.State = 0
        CheckEvent10
        AddScore ScoreRightRamp
        Exit Sub
    End If
    If YeyoStarted AND RightRampJackpotEnabled Then
        AddScore ScoreYeyoJackpot
        StopRightRampJackpot
        Exit Sub
    End If
    If TWIYStarted AND RightRampJackpotEnabled Then
        If LeftRampJackpotEnabled = 0 Then
            l25.State = 2
            l42.State = 2
        End If
        StopRightRampJackpot
        GiveTWIYJackpot
        Exit Sub
    End If
    If l43.State = 2 Then
        AddScore ScoreRightRamp
        l43.State = 1
        Exit Sub
    End If

    If l44.State = 2 Then
        AddScore ScoreRightRamp
        l44.State = 1
        Exit Sub
    End If

    If l45.State = 2 Then
        AddScore ScoreRightRamp
        l45.State = 1
        Exit Sub
    End If

    If l46.State = 2 Then
        AddScore ScoreRightRamp
        l46.State = 1
        Exit Sub
    End If

    If l47.State = 2 Then
        AddScore ScoreRightRamp
        l47.State = 1
        PrepareYeyoRun
    End If
End Sub

'**************
'   Yeyo Run
'**************
' 2 multiball
' lights 5, 24, 41

Sub PrepareYeyoRun
    If EventStarted = 11 Then Exit Sub
    l5.State = 2
    SideEventNr = 4
    PrepareSideMode
End Sub

Sub StartYeyoRun
    SideEventNr = 4
    l5.state = 2
    YeyoStarted = 1
    StartScarShooting 2 'shoot 2 multiballs
    BallsInDesk = 0
    StartBallSaved 20   '20 seconds
    ' StartRightRampJackpot
    l41.State = 0
    l24.State = 0
    l15.State = 2
    PlayTheme
End Sub

Sub EndYeyoRun
    If l5.State = 2 Then l5.State = 1
    If YeyoStarted Then
        YeyoHits = 0
        l43.State = 0
        l44.State = 0
        l45.State = 0
        l46.State = 0
        l47.State = 0
        YeyoLight = 0
        l15.State = 0
        YeyoStarted = 0
        StopRightRampJackpot
        EndSideMode
    End If
End Sub

Sub StartRightRampJackpot
    RightRampJackpotEnabled = 1
    LightSeqRightRamp.Play SeqRandom, 10, 10
	If B2SOn Then Controller.B2SStartAnimation("Muzzle") 
End Sub

Sub LightSeqRightRamp_PlayDone()
    LightSeqRightRamp.Play SeqRandom, 10, 10
End Sub

Sub StopRightRampJackpot
    RightRampJackpotEnabled = 0
    LightSeqRightRamp.StopPlay
	If B2SOn Then Controller.B2SStopAnimation("Muzzle")
End Sub

'************
' Get Car
'************
'20 seconds hurry up
' targets: rt1
' lights 17, 6

Dim CarHits, GetCarStarted, GetCarDelay

Sub InitGetCar
    CarHits = 0
    l17.State = 0
    l6.State = 0
    GetCarDelay = 0
    GetCarStarted = 0
End Sub

Sub CheckCarHits
    If EventStarted = 11 Then Exit Sub
    If EventStarted = 9 Then
        SetLamp 1, 0, 1, 5
        If CarHits = 5 Then
            WinEvent9
        End If
        Exit Sub
    End If
    If CarHits >= 10 Then 'PrepareGetCar
        CarHits = 0
        l6.State = 2
        SideEventNr = 5
        PrepareSideMode
    End If
End Sub

Sub StartGetCar
    Say "Run_while_you_can_stupid_fuck", "13131335142121431":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DD "", "", "dmdgetcar1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdgetcar2", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT THE CAR"), "", eNone, eBlinkFast, 0, 500, FALSE, ""
    DD "", "", "dmdgetcar1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdgetcar2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdgetcar1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdgetcar2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdgetcar1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdgetcar2", eNone, eNone, 0, 300, FALSE, ""
    GetCarDelay = 20 * 20
    l6.state = 2
    GetCarStarted = 1
    PlayTheme
    EjectBallsInDeskDelay = 80 'shoot back the ball
    l41.State = 0
    l24.State = 0
    l17.State = 2
    SetLamp 4, 0, 2, -1
    If B2SOn Then Controller.B2SStartAnimation("PalmTrees")
    SetLamp 3, 0, 1, -1
End Sub

Sub EndGetCar
    If l6.State = 2 Then l6.State = 1
    If GetCarStarted Then
        l6.State = 1
        l17.State = 0
        GetCarStarted = 0
        GetCarDelay = 0
        SetLamp 3, 0, 0, 0
        SetLamp 4, 0, 0, 0
        EndSideMode
    End If
End Sub

'***************
'   Mini Loop
'***************

'triggers: miniloop

Dim MiniLoopHits

Sub InitMiniLoops
    MiniLoopHits = 0
End Sub

Sub MiniLoop_Hit
    Dim tmp
    If EE Then
        Exit Sub
    End If
    If EventStarted = 10 Then
        Event10Hits(8) = 1
        l25.State = 0
        l42.State = 0
	CheckEvent10
        Exit Sub
    End If
    If TWIYStarted Then 'enable jackpot on the ramps
        l25.State = 0
        l42.State = 0
	tmp = INT(RND(1) * 5)
        Select case tmp
            Case 0:Say "Yeah1", "271":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 1:Say "Yeah2", "261":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 2:Say "Ok", "32341":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 3:Say "Thats_ok", "26351":If B2SOn Then Controller.B2SStartAnimation("Tony")
            Case 4:Say "Yeah_its_Tony", "3633371":If B2SOn Then Controller.B2SStartAnimation("Tony")
        End Select

        If LeftRampJackpotEnabled = 0 AND RightRampJackpotEnabled = 0 Then
            DD "", "", "dmdmultiball1", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdmultiball3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdmultiball1", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdmultiball3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdmultiball1", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdmultiball3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdmultiball1", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdmultiball3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdmultiball1", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdmultiball3", eNone, eNone, 0, 200, TRUE, ""
            StartLeftRampJackpot
            StartRightRampJackpot
            Exit Sub
        End If
    End If
    If RND(1) <0.5 Then
        PlaySound "ricochet1"
    Else
        PlaySound "ricochet2"
    End If
    ' normal mode
    AddScore ScoreMiniLoop
    If EventStarted = 11 Then Exit Sub
    MiniLoopHits = MiniLoopHits + 1
    CheckMiniLoop
End Sub

Sub CheckMiniLoop
    If EventStarted = 3 Then
        If MiniLoopHits > 2 Then
            MiniLoopHits = 0
            WinEvent3
        End If
        Exit Sub
    End If
    If MiniLoopHits >= 3 Then
        MiniLoopHits = 0
        OpenLockBlock 'enable lock
    End If
End Sub

'******************************
' The World if Yours Multiball
'******************************

'triggers: miniloop trigger, Ramp triggers for Jackpot
'lights: l4, l25, l42

Dim TWIYStarted

Sub InitTWIY
    TWIYStarted = 0
    l4.State = 0
End Sub

Sub StartTWIY
    DisplayFlushQueue
    DD "", "", "dmdmultiball1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball2", eNone, eNone, 0, 300, TRUE, ""
    l50.State = 0
    l51.State = 0
    l52.State = 0
    StartBallSaved 20
    SideEventStarted = 1
    SideEventNr = 3
    PlayTheme
    TWIYStarted = 1
    l4.State = 2
    l25.State = 2
    l42.State = 2
    BackDoorPost.IsDropped = 1                  'drop the post to enable the trigger for the mode
	DOF 136, DOFPulse
    StartScarShooting BallsInLock + BallsInDesk 'BallsinDesk should be 0, but just in case we eject all the balls
    BallsInLock = 0                             'empty the 3 locked balls
    BallsInDesk = 0                             'empty the desk balls
End Sub

Sub UpdateTWIYdmd
End Sub

Sub EndTWIY
    If l4.State = 2 Then l4.State = 1
    If TWIYStarted Then
	TWIYStarted = 0
        l4.State = 1
        l25.State = 0
        l42.State = 0
        StopLeftRampJackpot
        StopRightRampJackpot
        EndSideMode
    End If
End Sub

Sub GiveTWIYJackpot
    AddScore ScoreTWIYJackpot
    DisplayFlushQueue
    DD "", "", "dmdmultiball4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdmultiball7", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball4", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball7", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball4", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball7", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball4", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball7", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball4", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball7", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball4", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball7", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdmultiball4", eNone, eNone, 0, 150, FALSE, ""
    DD CenterTop(FormatScore(ScoreTWIYJackpot) ), "", "", eBlinkFast, eNone, 0, 800, True, ""
    ScoreTWIYJackpot = ScoreTWIYJackpot + 50000
    PlaySound "chanan3"
End Sub

'****************
'  Bank Targets
'****************

'targets: lt1, lt2, lt3, lt4
'lights: 11, 12, 13, 14 and l2

Dim BankTargets(4), BankDelay, BankStarted

Sub InitBank
    BankTargets(0) = 0
    BankTargets(1) = 0
    BankTargets(2) = 0
    BankTargets(3) = 0
    BankTargets(4) = 0
    l11.State = 0
    l12.State = 0
    l13.State = 0
    l14.State = 0
    l2.State = 0
    BankDelay = 0
    BankStarted = 0
End Sub

Sub CheckBank
    If EventStarted = 11 Then Exit Sub
    BankTargets(0) = BankTargets(1) + BankTargets(2) + BankTargets(3) + BankTargets(4)
    If BankTargets(0) = 4 Then
        AddScore ScoreBankLights
        l2.State = 2
        SideEventNr = 1
        PrepareSideMode
    End If
End Sub

Sub StartBank
    Say "Run_while_you_can_stupid_fuck", "13131335142121431":If B2SOn Then Controller.B2SStartAnimation("Tony")
    BankStarted = 1
    DD "", "", "dmdbankrun1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdbankrun2", eNone, eNone, 0, 300, FALSE, ""
    BankDelay = 20 * 20 '20 seconds
    l24.State = 0
    l41.State = 0
    EjectBallsInDeskDelay = 80
    PlayTheme
    SetLamp 3, 0, 2, -1
	If B2SOn Then Controller.B2SStartAnimation("PalmTrees") 
    LightSeqBankRun.Play SeqRandom, 50, , 4000
End Sub

Sub LightSeqBankRun_PlayDone()
    LightSeqBankRun.Play SeqRandom, 50, , 4000
End Sub

Sub EndBank
    If l2.State = 2 Then l2.State = 1
    If BankStarted Then
        BankStarted = 0
        SetLamp 3, 0, 0, 0
        LightSeqBankRun.StopPlay
        InitBank
        l2.State = 1
        EndSideMode
    End If
End Sub

'*******************
'    Main Events
'*******************
' slingshots increase the actual event if none is started
'lights 26,27,28,29,30,31,32,33,34,35,36
' EventStarted is set to 1 when any event is started

Dim Events(11), EventLights, CurrentEvent, EventStarted

Sub InitEvents
    Dim i
    EventLights = Array(dummy, l26, l27, l28, l29, l30, l31, l32, l33, l34, l35, l36)
    For i = 0 To 11
        Events(i) = 0

        EventLights(i).State = 0
    Next

    CurrentEvent = 1
    EventLights(CurrentEvent).State = 2
    EventStarted = 0
    Event1Delay = 0
    Event2Delay = 0
    Event3Delay = 0
    Event4Delay = 0
    Event5Delay = 0
    Event6Delay = 0
    Event7Delay = 0
    Event8Delay = 0
    Event9Delay = 0
    StartEvent11Delay = 0
    Event11VoiceDelay = 0
    EndEvent11Delay = 0
    EndWinEvent11Delay = 0
    rottHits = 0
    Event5Bumps = 0
    Event11Kill = 0
End Sub

Sub LightNextEvent
    If EventStarted Then Exit Sub     'if any event is started then do not change the lights
    If Events(CurrentEvent) = 0 Then 'the event is not finished so you may turn off the light :)
        EventLights(CurrentEvent).State = 0
    End If
    LightNextEvent2
End Sub

Sub LightNextEvent2
    CurrentEvent = CurrentEvent + 1
    If CurrentEvent = 12 Then CurrentEvent = 1
    If CurrentEvent = 11 AND EventsFinished <10 Then
        CurrentEvent = 1
    End If
    If Events(CurrentEvent) = 1 Then
        LightNextEvent2
    Else
        EventLights(CurrentEvent).State = 2
    End If
End Sub

Sub StartMainEvent
    If EventStarted OR EventsFinished = 10 Then ' another event is started so just return the ball
        EjectBallsInDeskDelay = 10              'shoot back the ball
    Else
			Select Case CurrentEvent
				Case 1:StartEvent1
				Case 2:StartEvent2
				Case 3:StartEvent3
				Case 4:StartEvent4
				Case 5:StartEvent5
				Case 6:StartEvent6
				case 7:StartEvent7
				case 8:StartEvent8
				case 9:StartEvent9
				case 10:StartEvent10
				case 11:StartEvent11
			End Select
		End If
		If B2SOn Then
			Controller.B2SStartAnimation("Logo")
		End If
End Sub

Sub EndMainEvent 'due to time out
		If EventStarted Then
			Select Case CurrentEvent
				Case 1:EndEvent1
				Case 2:EndEvent2
				Case 3:EndEvent3
				Case 4:EndEvent4
				Case 5:EndEvent5
				Case 6:EndEvent6
				case 7:EndEvent7
				case 8:EndEvent8
				case 9:EndEvent9
				case 10:EndEvent10
				case 11:EndEvent11
			End Select
		If B2SOn Then
			Controller.B2SStopAnimation("Logo")
		End If
		End If
End Sub

Sub ResetEvents 'make ready the next event after playing an event (winning or not)
    If EventsFinished = 3 Then LitExtraBall
    EventStarted = 0
    EventLights(CurrentEvent).State = 1
    ResetDroptargets
    LightNextEvent
    PlayTheme
End Sub

Function EventsFinished
    Dim i, tmp
    tmp = 0
    For i = 1 to 10
        tmp = tmp + Events(i)
    Next

    EventsFinished = tmp
End Function

Sub WinEventLights
    LightSeqWinEvent.Play SeqRandom, 60, , 1000
    GiOn
    SetLamp 1, 0, 1, 8
    SetLamp 2, 1, 0, 0
	If B2SOn Then Controller.B2SStartAnimation("PalmTrees") 
    SetLamp 3, 0, 1, 8
    SetLamp 4, 0, 1, 8
    SetLamp 5, 0, 1, 8
End Sub

'these are the lines displayed during the main events
'they are updated whenever the score changes
'otherwise they go in a loop until the event is finished

Sub UpdateEventsDMD
    DisplayFlushQueue
    Select Case EventStarted
        Case 1
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT KILL RAMP"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent1-1", eNone, eNone, 0, 600, FALSE, ""
            DD "", "", "dmdevent1-2", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent1-3", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent1-4", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent1-3", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent1-4", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent1-3", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent1-4", eNone, eNone, 0, 600, FALSE, ""
        Case 2
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 6 DROPTARGET"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent2-loop1", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop1", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop1", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop3", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop3", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent2-loop3", eNone, eNone, 0, 600, FALSE, ""
        Case 3
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 3 MINI LOOPS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent3-1", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent3-2", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent3-3", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent3-1", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent3-2", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent3-3", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent3-1", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent3-2", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent3-3", eNone, eNone, 0, 400, FALSE, ""
            DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent3-3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent3-3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent3-3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent3-3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
        Case 4
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent4-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent4-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent4-3", eNone, eNone, 0, 300, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent4-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent4-5", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent4-6", eNone, eNone, 0, 300, FALSE, ""
        Case 5
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 25 BUMPERS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent5-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-A", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-A", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-A", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 25 BUMPERS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent5-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-B", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-B", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-B", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 25 BUMPERS"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
        Case 6
            If Event6Drop Then
                DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT KILL RAMP"), "", eNone, eBlink, 0, 500, FALSE, ""
                DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
            Else
                DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT DROPTARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
                DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
                DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
            End If
        Case 7
            Select Case Event7Hits
                Case 0, 2, 4
                    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT BANK TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
                    DD "", "", "dmdevent7-1", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-2", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-3", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-4", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-4", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-4", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
                Case 1, 3, 5
                    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT RIGHT RAMP"), "", eNone, eBlink, 0, 500, FALSE, ""
                    DD "", "", "dmdevent7-1", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-2", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-3", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-4", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-4", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-4", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
                    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
                    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
            End Select
        Case 8
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("GET INTO THE CAR"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-5", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-6", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-5", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-6", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent8-3", eNone, eNone, 0, 300, FALSE, ""
        Case 9
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT THE CAR"), "", eNone, eBlink, 0, 500, FALSE, ""
            DD "", "", "dmdevent9-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
        Case 10
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT FLASH SHOTS"), "", eNone, eBlink, 0, 1500, FALSE, ""
            DD "", "", "dmdevent10-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent10-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent10-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent10-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent10-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent10-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent10-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent10-2", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent10-3", eNone, eNone, 0, 300, FALSE, ""
        Case 11
            DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("KILL TONY"), "", eNone, eBlinkFast, 0, 500, FALSE, ""
            DD "", "", "dmdevent11-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent11-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent11-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent11-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent11-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent11-3", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent11-1", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent11-2", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent11-3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent11-4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent11-1", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent11-2", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent11-3", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent11-4", eNone, eNone, 0, 200, FALSE, ""
            DD "", "", "dmdevent11-1", eNone, eNone, 0, 300, FALSE, ""
            DD "", "", "dmdevent11-3", eNone, eNone, 0, 300, FALSE, ""
    End Select
End Sub

'****************************
' Event 1 : Shoot kill ramp
'****************************
' triggers: LeftRampDone
' lights: l23

Dim Event1Delay

Sub StartEvent1
    Dim Balls
    Say "You_better_not_be_comunist", "12131313253423231413325272":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DD "", "", "dmdevent1-1", eNone, eNone, 0, 600, FALSE, ""
    DD "", "", "dmdevent1-2", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent1-3", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent1-4", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent1-3", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent1-4", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent1-3", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent1-4", eNone, eNone, 0, 600, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT KILL RAMP"), "", eNone, eBlink, 0, 2000, FALSE, ""
    EventStarted = 1
    l23.State = 2
    l37.State = 0
    l38.State = 0
    l39.State = 0
    l40.State = 0
    EjectBallsInDeskDelay = 80
    Event1Delay = 30 * 20 '30 seconds
    PlayTheme
End Sub

Sub WinEvent1
    DOF 137, DOFPulse    
    Say "fuckin_cockroach", "1934242624":If B2SOn Then Controller.B2SStartAnimation("Tony")
    WinEventLights
    Event1Delay = 0
    Events(1) = 1 'finished
    AddScore ScoreME1Complete
    l23.State = 0
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent1-7", eNone, eNone, 0, 600, FALSE, ""
    DD "", "", "dmdevent1-8", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, True, ""
End Sub

Sub EndEvent1
    Say "You_know_what_you_are_soft", "242673333574324242423241":If B2SOn Then Controller.B2SStartAnimation("Tony")  
    Event1Delay = 0
    Events(1) = 1 'finished event if it is not finished
    l23.State = 0
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent1-7", eNone, eNone, 0, 600, FALSE, ""
    DD "", "", "dmdevent1-8", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent1-10", eNone, eNone, 0, 200, True, ""
End Sub

'************************************
' Event 2 : shoot 5 center drop targets
'************************************
' triggers: ct1, ct2, ct3

Dim CTHits, Event2Delay

Sub StartEvent2
    Say "I_want_to_talk_2_George", "16873332362":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DD "", "", "dmdevent2-loop1", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop1", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop1", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop3", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop3", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop2", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent2-loop3", eNone, eNone, 0, 600, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 6 DROPTARGET"), "", eNone, eBlink, 0, 2000, FALSE, ""
    EventStarted = 2
	DOF 112, DOFPulse
    ct1.isdropped = 0
    ct2.isdropped = 0
    ct3.isdropped = 0
    PlaySoundat "fx_resetdrop", deskin
    l38.State = 2
    l39.State = 2
    l40.State = 2
    CTHits = 0
    EjectBallsInDeskDelay = 80
    Event2Delay = 45 * 20 '45 seconds
    PlayTheme
End Sub

Sub WinEvent2
    DOF 137, DOFPulse
    Say "Dont_fuck_with_me", "34433337":If B2SOn Then Controller.B2SStartAnimation("Tony")
    WinEventLights
    Event2Delay = 0
    Events(2) = 1
    AddScore ScoreME2Complete
    l37.State = 0
    l38.State = 0
    l39.State = 0
    l40.State = 0
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 600, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, True, ""
End Sub

Sub EndEvent2
    Say "You_boring_you_know_that", "153634151":If B2SOn Then Controller.B2SStartAnimation("Tony")
    Event2Delay = 0
    Events(2) = 1
    AddScore ScoreME2Part
    l37.State = 0
    l38.State = 0
    l39.State = 0
    l40.State = 0
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 600, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent2-final2", eNone, eNone, 0, 200, True, ""
End Sub

'************************************
' Event 3 : shoot 3 center mini loops
'************************************
' 3 loops in 45 seconds

Dim Event3Delay

Sub StartEvent3
    Say "Tony_would_like_a_word", "131312363628273414141":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DD "", "", "dmdevent3-1", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-2", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-3", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-1", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-2", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-3", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-1", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-2", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-3", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-3", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-3", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-3", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-3", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-4", eNone, eNone, 0, 200, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 3 MINI LOOPS"), "", eNone, eBlink, 0, 2000, FALSE, ""
    EventStarted = 3
	DOF 112, DOFPulse
    ct1.isdropped = 0
    ct2.isdropped = 0
    ct3.isdropped = 0
    PlaySoundat SoundFX("fx_resetdrop", DOFContactors), deskin
    InitMiniLoops
    l25.State = 2
    l42.State = 2
    EjectBallsInDeskDelay = 80
    Event3Delay = 45 * 20 '45 seconds
    PlayTheme
End Sub

Sub WinEvent3
    DOF 137, DOFPulse
    Say "Fuckin_fuck", "29381":If B2SOn Then Controller.B2SStartAnimation("Tony")
    WinEventLights
    Event3Delay = 0
    Events(3) = 1
    AddScore ScoreME3Complete
    l25.State = 0
    l42.State = 0
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent3-4", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-5", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-6", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-7", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-8", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 200, TRUE, ""
End Sub

Sub EndEvent3
    Say "You_fuckin_kidding_me_man", "2324133413141"
    Event3Delay = 0
    Events(3) = 1
    AddScore ScoreME3Part
    l25.State = 0
    l42.State = 0
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent3-4", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-5", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-6", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-7", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-8", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent3-10", eNone, eNone, 0, 200, TRUE, ""
End Sub

'************************************
' Event 4 : shoot the spinner
'************************************
' 50 hits in 45 seconds

Dim rottHits, Event4Delay

Sub StartEvent4
    Say "I_tell_you_something_fuck_you_man", "232414239535251":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DD "", "", "dmdevent4-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent4-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent4-3", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD "", "", "dmdevent4-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent4-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent4-6", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT 50"), "", eNone, eBlink, 0, 500, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SPINNER TARGETS"), "", eNone, eBlink, 0, 500, FALSE, ""
	DOF 112, DOFPulse
    ct1.isdropped = 0
    ct2.isdropped = 0
    ct3.isdropped = 0
    PlaySoundat SoundFX("fx_resetdrop", DOFContactors),deskin
    EventStarted = 4
    rottHits = 0
    l16.State = 2
    EjectBallsInDeskDelay = 80
    Event4Delay = 45 * 20 '45 seconds
    PlayTheme
End Sub

Sub WinEvent4
    DOF 137, DOFPulse
    Say "How_about_we_get_some_ice_cream", "2792131314141413131131":If B2SOn Then Controller.B2SStartAnimation("Tony")
    WinEventLights
    Event4Delay = 0
    Events(4) = 1
    AddScore ScoreME4Complete
    rottHits = 0
    l16.State = 0
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent4-7", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent4-8", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, TRUE, ""
End Sub

Sub EndEvent4
    Say "I_dont_give_a_fuck", "262423141":If B2SOn Then Controller.B2SStartAnimation("Tony")
    Event4Delay = 0
    Events(4) = 1
    AddScore ScoreME4Part
    rottHits = 0
    l16.State = 0
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent4-7", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent4-8", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 400, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-9", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent4-10", eNone, eNone, 0, 200, TRUE, ""
End Sub

Sub CheckrottHits
    rottHits = rottHits + 1
    If rottHits >= 50 Then
        WinEvent4
    End If
End Sub

'************************************
' Event 5 : shoot 50 bumpers
'************************************

Dim Event5Bumps, Event5Delay

Sub StartEvent5
    Say "You_know_what_you_need", "26279965342515351":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DD "", "", "dmdevent5-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-A", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-A", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-A", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 25 BUMPERS"), "", eNone, eBlink, 0, 1500, FALSE, ""
    DD "", "", "dmdevent5-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-B", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-B", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-B", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-4", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 25 BUMPERS"), "", eNone, eBlink, 0, 1500, FALSE, ""
    DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-final1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent5-final2", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT 25 BUMPERS"), "", eNone, eBlink, 0, 1500, FALSE, ""
    EventStarted = 5
    Event5Bumps = 0
    SetLamp 2, 1, 0, 0
    SetLamp 3, 0, 1, -1
        LightBumer1.State = 2
        LightBumer2.State = 2
        LightBumer3.State = 2
        LightBumer1a.State = 2
        LightBumer2a.State = 2
        LightBumer3a.State = 2
    l53.State = 2
    EjectBallsInDeskDelay = 80
    Event5Delay = 45 * 20 '45 seconds
    PlayTheme
End Sub

Sub WinEvent5
    DOF 137, DOFPulse
    Say "Boyfriend_fuck_him", "191589159218121813":If B2SOn Then Controller.B2SStartAnimation("Tony")
    WinEventLights
    DisplayFlushQueue
    Events(5) = 1
    Event5Delay = 0
    SetLamp 2, 0, 0, 0
    SetLamp 3, 0, 0, 0
    AddScore ScoreME5Complete
        LightBumer1.State = 0
        LightBumer2.State = 0
        LightBumer3.State = 0
        LightBumer1a.State = 0
        LightBumer2a.State = 0
        LightBumer3a.State = 0
    l53.State = 0
    ResetEvents
End Sub

Sub EndEvent5
    Say "Its_obvious_you_got_an_attitude", "131414242224231414141":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DisplayFlushQueue
    Events(5) = 1
    Event5Delay = 0
    SetLamp 2, 0, 0, 0
    SetLamp 3, 0, 0, 0
    AddScore ScoreME5Part
        LightBumer1.State = 0
        LightBumer2.State = 0
        LightBumer3.State = 0
        LightBumer1a.State = 0
        LightBumer2a.State = 0
        LightBumer3a.State = 0
    l53.State = 0
    ResetEvents
End Sub

Sub CheckEvent5Bumps
    Event5Bumps = Event5Bumps + 1
    If Event5Bumps >= 15 Then
        WinEvent5
    End If
End Sub

'*********************************************************
' Event 6 : drop 3 center droptargets and shoot left ramp
'*********************************************************

Dim Event6Drop, Event6Delay

Sub StartEvent6
    Say "You_fuckin_prick", "232423441":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT DROPTARGETS"), "", eNone, eBlink, 0, 1500, FALSE, ""
    EventStarted = 6
    'prepare drop targets
    PlaySoundat SoundFX("fx_resetdrop", DOFContactors), deskin
    DroppedTargets(0) = 0
    DroppedTargets(1) = 0
    DroppedTargets(2) = 0
    DroppedTargets(3) = 0
	DOF 112, DOFContactors
    ct1.isdropped = 0
    ct2.isdropped = 0
    ct3.isdropped = 0
    l38.State = 2
    l39.State = 2
    l40.State = 2
    l37.State = 0
    Event6Drop = 0
    EjectBallsInDeskDelay = 80
    Event6Delay = 45 * 20 '45 seconds
    PlayTheme
End Sub

Sub WinEvent6
    DOF 137, DOFPulse
    Say "Who_you_think_you_fuckin_with", "1324313131313761":If B2SOn Then Controller.B2SStartAnimation("Tony")
    WinEventLights
    Events(6) = 1
    Event6Delay = 0
    l23.State = 0
    AddScore ScoreME6Complete
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-7", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-7", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-7", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-6", eNone, eNone, 0, 300, TRUE, ""
End Sub

Sub EndEvent6
    Say "You_just_fucked_up_man", "24251423341":If B2SOn Then Controller.B2SStartAnimation("Tony")
    Events(6) = 1
    Event6Delay = 0
    l23.State = 0
    AddScore ScoreME6Part
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent6-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-7", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-7", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent6-7", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent6-6", eNone, eNone, 0, 300, TRUE, ""
End Sub

Sub CheckEvent6
    DroppedTargets(0) = DroppedTargets(1) + DroppedTargets(2) + DroppedTargets(3)
    If DroppedTargets(0) = 3 Then 'enable the left ramp score
	l23.State = 2
        l38.State = 0
        l39.State = 0
        l40.State = 0
        Event6Drop = 1
        UpdateEventsDMD
    End If
End Sub

'************************************************
' Event 7: shoot right ramp and left bank targets
'************************************************

Dim Event7State, Event7Hits, Event7Delay

Sub StartEvent7
    Say "What_are_you_going_to_do_for_me", "1312121312131":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DisplayFlushQueue
    DD "", "", "dmdevent7-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent7-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent7-6", eNone, eNone, 0, 200, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT BANK TARGETS"), "", eNone, eBlink, 0, 1500, FALSE, ""
    EventStarted = 7
    Event7Hits = 0
    PrepareBank
    EjectBallsInDeskDelay = 80
    Event7Delay = 45 * 20 '45 seconds
    PlayTheme
End Sub

Sub WinEvent7
    DOF 137, DOFPulse
    Say "I_like_you_your_like_a_tiger", "2713931234131":If B2SOn Then Controller.B2SStartAnimation("Tony")
    WinEventLights
    Events(7) = 1
    Event7Delay = 0
    InitBank
    l48.State = 0
    ResetEvents
    DisPlayScoreNow
    AddScore ScoreME7Complete
End Sub

Sub EndEvent7
    Say "Go_fuck_yourself", "353528":If B2SOn Then Controller.B2SStartAnimation("Tony")
    Events(7) = 1
    Event7Delay = 0
    InitBank
    l48.State = 0
    ResetEvents
    DisPlayScoreNow
    AddScore ScoreME7Part
End Sub

Sub CheckEvent7
    Select Case Event7Hits
        Case 0 'bank
            If Event7State = 0 Then
                Event7Hits = 1
                PrepareRRamp
            End If
        Case 1 'Right ramp
            If Event7State = 1 Then
                Event7Hits = 2
                PrepareBank
            End If
        Case 2 'bank
            If Event7State = 0 Then
                Event7Hits = 3
                PrepareRRamp
            End If
        Case 3 'Right ramp win
            If Event7State = 1 Then
                WinEvent7
            End If
    End Select
End Sub

Sub PrepareBank
    Event7State = 0 'bank
    l11.State = 2
    l12.State = 2
    l13.State = 2
    l14.State = 2
    l48.State = 0
End Sub

Sub PrepareRRamp
    Event7State = 1 'right ramp
    l11.State = 0
    l12.State = 0
    l13.State = 0
    l14.State = 0
    l48.State = 2
End Sub

'************************************
' Event 8 : car skillshot
'************************************
Dim Event8Delay

Sub StartEvent8
    Say "Have_my_car_set_to_me_ok", "24152414141726":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DisplayFlushQueue
    DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent8-3", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("GET INTO THE CAR"), "", eNone, eBlink, 0, 1500, FALSE, ""
    EventStarted = 8
    LeftFlipper.RotateToStart
    RightFlipper.RotateToStart
    l17.State = 2
    SetLamp 4, 1, 0, 0
    Event8Delay = 30 * 20 '30 seconds
    BallsInDesk = BallsInDesk - 1
    NewBall8
    PlayTheme
End Sub

Sub EndEvent8
    Say "Bring_it_to_me_cockroach", "1622111115121":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DisPlayScoreNow
    Events(8) = 1
    Event8Delay = 0 'just to be sure it is 0
    l17.State = 0
    SetLamp 4, 0, 0, 0
    ResetEvents
End Sub

Sub NewBall8
    If EventStarted = 8 Then AutoPlungerReady = 0
    If Event8Delay = 0 Then EndEvent8
    BallRelease.createball
    BallsOnPlayfield = BallsOnPlayfield + 1
    BallRelease.Kick 90, 8
    PlaySoundat "fx_ballrel", ballrelease
End Sub

'************************************
' Event 9 : 5 car hits
'************************************
Dim Event9Delay

Sub StartEvent9
    Say "My_name_is_Tony_M", "2414133413341":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DisplayFlushQueue
    DD "", "", "dmdevent9-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-4", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("SHOOT THE CAR"), "", eNone, eBlink, 0, 1500, FALSE, ""
    EventStarted = 9
    CarHits = 0
    l17.State = 2
    EjectBallsInDeskDelay = 80
    Event9Delay = 45 * 20 '45 seconds
    PlayTheme
End Sub

Sub WinEvent9
    DOF 137, DOFPulse
    Say "I_told_you_not_to_fuck_with_me", "2335145336543332151":If B2SOn Then Controller.B2SStartAnimation("Tony")
    WinEventLights
    Event9Delay = 0
    Events(9) = 1
    CarHits = 0
    l17.State = 0
    AddScore ScoreME9Complete
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent9-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-7", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 150, TRUE, ""
End Sub

Sub EndEvent9
    Say "Shit_ass_fuck", "362434":If B2SOn Then Controller.B2SStartAnimation("Tony")
    Event9Delay = 0
    Events(9) = 1
    CarHits = 0
    l17.State = 0
    AddScore ScoreME9Part
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent9-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-7", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-8", eNone, eNone, 0, 150, FALSE, ""
    DD "", "", "dmdevent9-9", eNone, eNone, 0, 150, TRUE, ""
End Sub

'************************************
' Event 10 : shoot all shots
'************************************
' not timed. Ends when ball drains

Dim Event10Hits(8)

Sub StartEvent10
    Dim i
    Say "Ok_so_why_dont_we_get_together", "322384121413141":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DisplayFlushQueue
    DD "", "", "dmdevent10-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-2", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-3", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("HIT FLASH SHOTS"), "", eNone, eBlink, 0, 1500, FALSE, ""
    EventStarted = 10
    For i = 0 to 8
        Event10Hits(i) = 0
    Next

    l11.State = 2
    l12.State = 2
    l13.State = 2
    l14.State = 2
    l16.State = 2
    l17.State = 2
    l23.State = 2
    l25.State = 2
    l37.State = 2
    l38.State = 2
    l39.State = 2
    l40.State = 2
    l42.State = 2
    l48.State = 2
    l53.State = 2
    EjectBallsInDeskDelay = 80
    PlayTheme
End Sub

Sub WinEvent10
    DOF 137, DOFPulse
    Say "Yeah_its_Tony", "3633371":If B2SOn Then Controller.B2SStartAnimation("Tony")
    WinEventLights
    l16.State = 0
    l17.State = 0
    l23.State = 0
    l25.State = 0
    l37.State = 0
    l42.State = 0
    l48.State = 0
    l53.State = 0
    Events(10) = 1
    AddScore ScoreME10Complete
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent10-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, TRUE, ""
End Sub

Sub EndEvent10
	Say "Im_fucked", "5417191":If B2SOn Then Controller.B2SStartAnimation("Tony")
    l16.State = 0
    l17.State = 0
    l23.State = 0
    l25.State = 0
    l37.State = 0
    l42.State = 0
    l48.State = 0
    l53.State = 0
    Events(10) = 1
    AddScore ScoreME10Part
    ResetEvents
    DisplayFlushQueue
    DD "", "", "dmdevent10-4", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-7", eNone, eNone, 0, 100, FALSE, ""
    DD "", "", "dmdevent10-6", eNone, eNone, 0, 100, TRUE, ""
End Sub

Sub CheckEvent10
    Dim i
    Event10Hits(0) = 0
    For i = 1 to 8
        Event10Hits(0) = Event10Hits(0) + Event10Hits(i)
    Next

    Select Case Event10Hits(0)
        Case 1:PlaySound "chanan5"
        Case 2:PlaySound "chanan5A"
        Case 3:PlaySound "chanan5B"
        Case 4:PlaySound "chanan5C"
        Case 5:PlaySound "chanan5D"
        Case 6:PlaySound "chanan5C"
        Case 7:PlaySound "chanan5D"
        Case 8:PlaySound "chanan2":WinEvent10
    End Select
End Sub

'*******************************************
' Event 11 : shoot 10 Desk shots + multiball
'*******************************************

Dim DeskHits, StartEvent11Delay, Event11VoiceDelay, EndEvent11Delay, Event11Kill, EndWinEvent11Delay

Sub StartEvent11
    Dim i
    For i = 0 To 11
        EventLights(i).State = 0
    Next
 
    Say "sayhello2", "1415142926161":If B2SOn Then Controller.B2SStartAnimation("Tony")
    DisplayFlushQueue
    DD "", "", "dmdevent11-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-3", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent11-2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent11-3", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent11-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent11-1", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent11-2", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent11-3", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent11-4", eNone, eNone, 0, 200, FALSE, ""
    DD "", "", "dmdevent11-1", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-3", eNone, eNone, 0, 300, FALSE, ""
    DD CenterTop(FormatScore(Score(player) ) ), CenterBottom("KILL TONY"), "", eNone, eBlink, 0, 1500, FALSE, ""
    EventStarted = 11
    DropAll        ' drop all targets
    BackDoorPost.IsDropped = 1:DOF 136, DOFPulse
    CloseLockBlock 'close the lock if it was open
    l36.State = 2
    l37.State = 2
    l38.State = 0
    l39.State = 0
    l40.State = 0
	If B2SOn Then Controller.B2SStartAnimation("PalmTrees") 
    SetLamp 3, 0, 2, -1
    DeskHits = 0
    Event11Kill = 0
    StartScarShooting 4
    BallsInDesk = 0
    Event11VoiceDelay = 700
    StartBallSaved 20          '20 seconds ball saved
    EndEvent11Delay = 180 * 20 '3 minutes to complete the event
    PlayTheme
End Sub

Sub WinEvent11
    'simulate tilt to turn all lights and flippers off
	DOF 137, DOFPulse
    Tilted = True
    DOF 210, DOFOff
    GiOff
    LightSeqTilt.Play SeqAllOff
    TiltObjects 1
    'eject balls under the desk
    EjectBallsInDeskDelay = 20
    BallsRemaining = BallsRemaining + 1 'increase one ball to replay the last ball
    PlaySound "tonydeath"
    DisplayFlushQueue
    DD "", "", "dmdevent11-5", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-6", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-7", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-8", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-9", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-10", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-11", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-12", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-13", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-14", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-15", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-16", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-17", eNone, eNone, 0, 300, FALSE, ""
    DD "", "", "dmdevent11-18", eNone, eNone, 0, 1500, FALSE, ""
    DD "", "", "dmdevent11-18-2", eNone, eNone, 0, 1500, FALSE, ""
    DD "", "", "dmdevent11-19", eNone, eNone, 0, 1500, TRUE, ""
    Event11VoiceDelay = 0
    EndEvent11Delay = 0
    EndWinEvent11Delay = 240
    l37.State = 0
    l25.State = 0
    l42.State = 0
    l24.State = 0
    l41.State = 0
End Sub

Sub EndWinEvent11
    SetLamp 3, 0, 0, 0
    DisplayScoreNow
    ResetDropTargets
    InitEvents
    PlayTheme
End Sub

Sub EndEvent11 'executed from the drain when all multiballs drained
    SetLamp 3, 0, 0, 0
    Say "saygoodnight", "251414152424231":If B2SOn Then Controller.B2SStartAnimation("Tony")
    Event11VoiceDelay = 0
    DisplayScoreNow
    ResetDropTargets
    InitEvents
    l37.State = 0
    l25.State = 0
    l42.State = 0
    l24.State = 0
    l41.State = 0
    PlayTheme
End Sub

Sub NextPartEvent11
    ResetDropTargets
    l25.State = 2
    l42.State = 2
    l24.State = 2
    l41.State = 2
    l37.State = 0
    l38.State = 0
    l39.State = 0
    l40.State = 0
    BackDoorPost.IsDropped = 0:DOF 136, DOFPulse
    Event11Kill = 1
    StartScarShooting BallsInDesk
    BallsInDesk = 0
End Sub

Sub SayEvent11
    Dim tmp
    tmp = INT(6 * RND(1) )
    Select Case tmp
        Case 0:Say "You_know_who_your_fuckin_with", "25162424261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 1:Say "You_need_an_army_to_take_me", "13151414141413331":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 2:Say "You_picked_the_wrong_guy_to_fuck_with", "23232523233334261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 3:Say "You_think_you_can_fuck_with_me", "2414141423261":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 4:Say "Cmon_come_and_get_me", "12132643233414":If B2SOn Then Controller.B2SStartAnimation("Tony")
        Case 5:Say "Cmon_bring_your_army", "13144814":If B2SOn Then Controller.B2SStartAnimation("Tony")
    End Select
End Sub

'*******************************
' Easter Egg: shoot 100 targets
'*******************************
' 3 ball multiball, ballsave 1 minute, hit 100 targets

Dim EE, TempEgg, EndEEDelay, EndEEDelay2, StartEasterEggShootingDelay

Sub InitEasterEgg
    TempEgg = ""
    EE = 0
    EndEEDelay = 0
    EndEEDelay2 = 0
    StartEasterEggShootingDelay = 0
End Sub

Sub CheckStartEgg
    TempEgg = RIGHT(TempEgg, 5)
    If TempEgg = "rlrll" AND Credits> 0 Then
        StartEasterEgg
    End If
End Sub

Sub StartEasterEgg
    DisplayFlushQueue
    DD CenterTop("YOU FOUND"), CenterBottom("THE EASTER EGG"), "", eNone, eBlink, 0, 1000, True, ""
    'reset everything
    Initialize
    AttractMode_Off
    GiOn
    Player = 0
    StartEasterEggShootingDelay = 50
    StartBallSaved 61
    EndEEDelay = 120 * 20
    PlaySong "bgout_scarface_EE.mp3"
    EE = 1        'easter egg is active
    l26.State = 0 'turn off the first event light
	If B2SOn Then Controller.B2SStartAnimation("PalmTrees") 
    SetLamp 3, 0, 1, -1
    CTDelay = 0
    EasterLights
End Sub

Sub EasterLights
    LightSeqEE.Play SeqUpOn, 50, , 1
End Sub

Sub EasterLightsOff
    LightSeqEE.StopPlay
End Sub

Sub StartEasterEggShooting
    StartScarShooting 3
    BallsinDesk = 0
End Sub

Sub PlayRamdomPunch
    Dim i
    i = INT(RND(1) * 3)
    Select case i
        Case 0:PlaySound "punch1"
        Case 1:PlaySound "punch1"
        Case 2:PlaySound "punch1"
    End Select
End Sub

Sub EndEE
    GameStarted = 0
	DOF 121, DOFPulse
    AllLampsOff
    GiOff
    LeftFlipper.RotateToStart
    RightFlipper.RotateToStart
    If Score(0) >= 200 Then
        'addcredit
        credits = credits + 1:DOF 132, DOFOn
        DD CenterTop("YOU WIN"), "", "", eNone, eBlink, 0, 1000, True, "sf_youwin"
		DOF 137, DOFPulse
        savehs()
    Else
        credits = credits - 1
        DD CenterTop("YOU LOSE"), "", "", eNone, eBlink, 0, 1000, True, "sf_youlose"
    End If
    EndEEDelay2 = 50
    SetLamp 3, 0, 0, 0
    EasterLightsOff
	DOF 222, DOFOff
End Sub


'******************************************************************
' JP's Fading Lamps for Original tables
' v7.0 for VP9 Only Fading Lights
' Used mostly for flash effects
' Based on PD's Fade Lights System
'******************************************************************
' Syntax:
' SetLamp nr, a, b, c
' a 0 is Off, 1 is On
' b 0 is no blink, 1 is blink, 2 is fast blink
' c number of blinks, -1 is until stopped
' LState(x) final lamp state
' LFade(x)  Fading step
' LBlink(x) nr of blinks
' example:
' setlamp 64, 0, 2, 10
' this is turn off the lamp 64 but first it blinks fast 10 times
' setlamp 64, 1, 2, 10
' this is turn on the lamp 64 but first it blinks fast 10 times
'******************************************************************

Dim LState(200), LFade(200), LBlink(200)

ForceAllLampsOff()
LampTimer.Interval = 40
LampTimer.Enabled = 1

Sub LampTimer_Timer()

	FadeL 1, Light001 'Red Dome

	FadeL 2, Light002 'BackWall Dome

	FadeL 3, Light003 'BackWall Bulbs

	FadeLm 4, Light004a 'Car Lights
	FadeL 4, Light004b 'Car Lights

	FadeLm 5, Light005a 'Lamp Posts
	FadeLm 5, Light005b 'Lamp Posts
	FadeLm 5, Light005c 'Lamp Posts
	FadeLm 5, Light005d 'Lamp Posts
	FadeL 5, Light005e 'Lamp Posts


End Sub

Sub AllLampsOff()
    Dim x
    For x = 0 to 200
        SetLamp x, 0, 0, 0
    Next
End Sub

Sub ForceAllLampsOff()
    Dim x
    For x = 0 to 200
        LFade(x) = 8
    Next
End Sub

Sub SetLamp(nr, value, blink, repeat)
    If value> 1 or blink> 2 Then Exit Sub
    If blink = 0 AND value = 0 AND LFade(nr) = 0 Then Exit Sub 'the lamp is already Off and there is no blink so do nothing
    LState(nr) = value
    LBlink(nr) = repeat
    LFade(nr) = value + 2 * blink + 2
    Select Case LFade(nr)
        Case 2:LFade(nr) = 8     'off
        Case 3:LFade(nr) = 11    'on
        Case 4, 5:LFade(nr) = 12 'Blink
        Case 6, 7:LFade(nr) = 16 'Blink fast
    End Select
End Sub

'Lights used 

Sub FadeL(nr, a)
    Select Case LFade(nr)
        'fade to off
        Case 8:a.State = 0:LFade(nr) = 9
        Case 9:'b.State = 1:LFade(nr) = 10
        Case 10:'b.State = 0:LFade(nr) = 0
        'turn on
        Case 11:a.State = 0:a.State = 1:LFade(nr) = 1
        'blink
        Case 12:a.State = 1:LFade(nr) = 13
        Case 13:a.State = 0:LFade(nr) = 14
        Case 14:'b.State = 1:LFade(nr) = 15
        Case 15:'b.State = 0
            LBlink(nr) = LBlink(nr) -1
            If LBlink(nr) = 0 Then
                LFade(nr) = 8 + 3 * LState(nr)
            Else
                LFade(nr) = 12
            End If
        'blink fast
        Case 16:a.State = 1:LFade(nr) = 17
        Case 17:a.state = 0
            LBlink(nr) = LBlink(nr) -1
            If LBlink(nr) = 0 Then
                LFade(nr) = 8 + 3 * LState(nr)
            Else
                LFade(nr) = 16
            End If
    End Select
End Sub

Sub FadeLm(nr, a)
    Select Case LFade(nr)
        'fade to off
        Case 8:a.State = 0
        Case 9:'b.State = 1
        Case 10:'b.State = 0
        'turn on
        Case 11:a.State = 0:a.State = 1
        'blink
        Case 12:a.State = 1
        Case 13:a.State = 0
        Case 14:'b.State = 1
        Case 15:'b.State = 0
        'blink fast
        Case 16:a.State = 1
        Case 17:a.State = 0
    End Select
End Sub

Sub LSample_Timer()

	Flasher2b.visible = Light002.state

	Flasher3a.visible = Light003.state
	Flasher3b.visible = Light003.state
	Flasher3c.visible = Light003.state
	Flasher3d.visible = Light003.state
	Flasher3e.visible = Light003.state
	Flasher3f.visible = Light003.state

	Flasher4a.visible = Light004a.state
	Flasher4b.visible = Light004a.state

	Flasher5a.visible = Light005a.state
	Flasher5b.visible = Light005a.state
	Flasher5c.visible = Light005a.state
	Flasher5d.visible = Light005a.state
	Flasher5e.visible = Light005a.state

IF Light001.State = 1 Then
	Prim1a.image = "dome2_0_redON"
Else
	Prim1a.image = "dome2_0_red"
End IF


IF Light003.State = 1 Then
	Prim3a.image = "BulbON"
	Prim3b.image = "BulbON"
	Prim3c.image = "BulbON"
	Prim3d.image = "BulbON"
	Prim3e.image = "BulbON"
	Prim3f.image = "BulbON"
Else
	Prim3a.image = "BulbOFF"
	Prim3b.image = "BulbOFF"
	Prim3c.image = "BulbOFF"
	Prim3d.image = "BulbOFF"
	Prim3e.image = "BulbOFF"
	Prim3f.image = "BulbOFF"
End IF


End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'********************************************
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'********************************************

Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)

		aBallShadow(b).X = BOT(b).X
		aBallShadow(b).Y = BOT(b).Y

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz)/17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

