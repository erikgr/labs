/*
 * SMS proxy relay for arduino GSM shield.
 *
 * 2016 :: Neko
 */

#include <GSM.h>
#include <string.h>


#define PINNUMBER "1234"
#define MYNUM "CENSORED"
#define SMS_BUF_LEN 140
#define TFN_NUMBER_LEN 15
#define RELAY_BEGIN_CHAR '<'
#define RELAY_END_CHAR '>'


GSM gsm;
GSM_SMS sms;


void relay_sms(char *dst_num, char *src_num, char *msg){
  sms.beginSMS(dst_num);
  sms.print("Relayed message from: ");
  sms.print(src_num);
  sms.print("\n");
  sms.print(msg);
  sms.endSMS();
}

void send_sms(char *dst_num, char *msg){
  sms.beginSMS(dst_num);
  sms.print(msg);
  sms.endSMS();
}


int find_char(char *arr, int len, char c) {
  int i;
  for(i=0; i<len; i++){
    if(arr[i] == c){ return i; }
  }
  return -1;
}

void setup() {

  Serial.begin(9600);
  Serial.println("SMS Proxy");

  boolean connected = false;
  
  while (!connected) {
    Serial.println("Connecting..");
    if (gsm.begin(PINNUMBER) == GSM_READY){
      connected = true;
    } else {
      Serial.println("Connection failed. Retrying..");
      delay(1000);
    }
  }

  Serial.println("Connected.");
}

void loop() {

  char src_num[TFN_NUMBER_LEN] = { '\0' };
  char dst_num[TFN_NUMBER_LEN] = { '\0' };
  char sms_msg[SMS_BUF_LEN] = { '\0' };
  char buf[SMS_BUF_LEN] = { '\0' };
  char *p = buf;
  char c;

  if (sms.available()) {

    sms.remoteNumber(src_num, TFN_NUMBER_LEN);
    boolean relay = (sms.peek() == RELAY_BEGIN_CHAR);

    while(c = sms.read()){ *p = c; p++; }

    int n_begin = find_char(buf, SMS_BUF_LEN, RELAY_BEGIN_CHAR) + 1;
    int n_end = find_char(buf, SMS_BUF_LEN, RELAY_END_CHAR);

    if(n_end == -1){ relay = false; }

    if(relay){
      strncpy(dst_num, buf + n_begin, (n_end - n_begin));
      strncpy(sms_msg, buf + (n_end + 1), SMS_BUF_LEN);
      send_sms(dst_num, sms_msg);
    } else {
      strncpy(dst_num, MYNUM, strlen(MYNUM));
      strncpy(sms_msg, buf, strlen(buf));
      relay_sms(dst_num, src_num, sms_msg);
    }

    Serial.print("SRC_NUM: ");
    Serial.println(src_num);
    Serial.print("DST_NUM: ");
    Serial.println(dst_num);
    Serial.print("SMS_MSG: ");
    Serial.println(sms_msg);

    sms.flush();
  }

  delay(1000);
}

