CREATE TABLE key_ (
  Store_number NUMBER,
  Weather_station_number NUMBER,
  CONSTRAINT pk_key PRIMARY KEY (Store_number, Weather_station_number),
  CONSTRAINT fk_store FOREIGN KEY (Store_number) REFERENCES PRODUCT (STORE_NBR),
  CONSTRAINT fk_weather FOREIGN KEY (Weather_station_number) REFERENCES weather (STATION_NBR));