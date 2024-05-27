
/**
* MQTT-Serial 透传程序
* @author SummerGao
* Created : 22. 5月 2024 22:00
*/

#include <ESP8266WiFi.h>
#include <PubSubClient.h>
#include <ESP8266WiFiMulti.h>

//WiFi相关配置，候选Wifi
const char* ssid_1 = "SummerGao1"; //WiFi名称
const char* password_1 = "123456";//WiFi密码
const char* ssid_2 = "SummerGao2"; //WiFi名称
const char* password_2 = "123456";//WiFi密码
const char* ssid_3 = "SummerGao3"; //WiFi名称
const char* password_3 = "123456";//WiFi密码
//MQTT Client相关配置
const char* mqtt_server = "localhost"; //MQTT服务器地址
const char* mqtt_username = "SummerGao"; //MQTT用户名
const char* mqtt_password = "123456"; //MQTT密码
const char* TOPIC = "home/devices/onoff/"; // 订阅消息主题
const char* TOPIC_WAKEUP = "home/devices/wake_up/"; // 订阅消息主题
const char* client_id = "clientId-ApjJZcy2024Dh";  // 标识当前设备的客户端编号

// WiFi connect timeout per AP. Increase when connecting takes longer.
const uint32_t connectTimeoutMs = 5000;
ESP8266WiFiMulti wifiMulti;

WiFiClient espClient;                             // 定义wifiClient实例
PubSubClient client(espClient);                   // 定义PubSubClient的实例
long lastMsg = 0;                                 // 记录上一次发送信息的时长

void setup() {
  pinMode(BUILTIN_LED, OUTPUT);                   // 定义板载LED灯为输出方式
  pinMode(D8, OUTPUT);                            //D8引脚连接串口墨水屏WAKE引脚
  Serial.begin(115200);
  //D4->TX引脚
  Serial1.begin(115200);
  // 板子通电后要启动，稍微等待一下让板子点亮
  delay(10);
  setup_wifi();                                    //执行Wifi初始化，下文有具体描述
  client.setServer(mqtt_server, 1883);             //设定MQTT服务器与使用的端口，1883是默认的MQTT端口
  client.setCallback(callback);                    //设定回调方式，当ESP8266收到订阅消息时会调用此方法
}

void setup_wifi() {
  // Don't save WiFi configuration in flash - optional
  WiFi.persistent(false);
  // Register multi WiFi networks
  wifiMulti.addAP(ssid_1, password_1);
  wifiMulti.addAP(ssid_2, password_2);
  wifiMulti.addAP(ssid_3, password_3);
  // More is possible

  // Maintain WiFi connection
  if (wifiMulti.run(connectTimeoutMs) == WL_CONNECTED) {
    Serial.print("WiFi connected: ");
    Serial.println(WiFi.SSID());
    Serial.print("IP address: ");
    Serial.println(WiFi.localIP());
  } else {
    Serial.println("WiFi not connected!");
  }

}

//\xA5\x00\x09\x0A\xCC\x33\xC3\x3C\xA6
//A500090ACC33C33CA6
void callback(char* topic, byte* payload, unsigned int length) {
  //Serial.println(topic);
  if (strcmp(topic, TOPIC_WAKEUP) == 0) {
    String message = "";
    for (int i = 0; i < length; i++)
    {
      message += (char)payload[i];
    }

    if (message == "WAKE_UP_0") {
      digitalWrite(D8, LOW);
      Serial.println(message);
    } else if (message == "WAKE_UP_1") {
      digitalWrite(D8, HIGH);
      Serial.println(message);
    }
  } else if (strcmp(topic, TOPIC) == 0) {
    char x;
    for (int i = 0; i < length; i++)
    {
      x = (char)payload[i];
      //串口0：打印指令消息
      Serial.write(x);
      //串口1：转发指令消息
      Serial1.write(x);
    }
  } else {

  }

  if ((char)payload[0] == '1') {
    digitalWrite(BUILTIN_LED, HIGH);   // 亮灯
  } else {
    digitalWrite(BUILTIN_LED, LOW);   // 熄灯
  }
}

void reconnect() {
  while (!client.connected()) {
    Serial.print("Attempting MQTT connection...");
    // Attempt to connect
    if (client.connect(client_id, mqtt_username, mqtt_password)) {
      Serial.println("connected");
      // 连接成功时订阅主题
      client.subscribe(TOPIC);
      client.subscribe(TOPIC_WAKEUP);
    } else {
      Serial.print("failed, rc=");
      Serial.print(client.state());
      Serial.println(" try again in 5 seconds");
      //重置wifi连接
      setup_wifi();
      // Wait 5 seconds before retrying
      delay(5000);
    }
  }
}

void loop() {

  if (!client.connected()) {
    reconnect();
  }
  client.loop();

  long now = millis();
  if (now - lastMsg > 2000) {
    lastMsg = now;
    client.publish("home/status/", "{device:client_id,'status':'on'}");
  }
}
