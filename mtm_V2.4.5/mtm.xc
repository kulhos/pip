/home/pip/pip_V02/mtm_V2.4.5/mtmapi.sl
ClConnect: void ClConnect(I:xc_char_t *,IO:xc_int_t *)
ClDisconnect: void ClDisconnect(IO:xc_int_t *)
ClExchmsg: void ClExchmsg(IO:xc_string_t *,IO:xc_string_t *,I:xc_int_t,IO:xc_int_t *)
ClSendMsg: void ClSendMsg(IO:xc_string_t *,I:xc_int_t,IO:xc_int_t *)
ClGetMsg: void ClGetMsg(IO:xc_string_t *,I:xc_int_t,IO:xc_int_t *)
SrvConnect: void SrvConnect(I:xc_char_t *,IO:xc_int_t *,IO:xc_int_t *)
SrvDisconnect: void SrvDisconnect()
SrvGetMsg: void SrvGetMsg(IO:xc_string_t *,I:xc_int_t,IO:xc_int_t *)
SrvReply: void SrvReply(IO:xc_string_t *,IO:xc_int_t *)
SrvMTMId: void SrvMTMId(I:xc_char_t *,IO:xc_string_t *)
MTMCntrl: void MTMCntrl(IO:xc_char_t *,IO:xc_string_t *,I:xc_char_t *,IO:xc_char_t **,IO:xc_int_t *)
MTMRunning: void MTMRunning(I:xc_char_t *,O:xc_int_t *)
