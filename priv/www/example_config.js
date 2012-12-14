// configure the test here
var TestData = {
  "TestName": "MUSHRA Test",
  "TestId": "mobileToPc",
  "RateScalePng": "img/scale_abs.png",
  "RateScaleBgPng": "img/scale_abs_background.png",
  "RateMinValue": 0,
  "RateMaxValue": 100,
  "RateDefaultValue":0,
  "LoopByDefault": false,
  "ShowFileIDs": false,
  "EnableABLoop": true,
  "EnableOnlineSubmission": true,
  "SubmitResultsURL": "log",
  "SupervisorContact": "hangloung.lee@nhn.com",
  "Testsets": [
    {
      "Name": "mobileToPc",
      "Reference": "mobile_to_pc/reference.wav",
      "Files": [
            "mobile_to_pc/i4_to_mac.wav",
            "mobile_to_pc/mac_to_win.wav",
            "mobile_to_pc/s2_to_mac.wav",
            "mobile_to_pc/win_to_mac.wav"
        ]
    }
  ]
}
