CREATE TABLE PRODUCT
(
  SALE_DATE DATE NOT NULL,
  STORE_NBR NUMBER NOT NULL,
  ITEM_NBR NUMBER NOT NULL,
  UNITS NUMBER, 
  
  CONSTRAINT pk_product PRIMARY KEY (STORE_NBR, ITEM_NBR)
);

CREATE TABLE PRODUCT_WEATHER_KEY
(
  STORE_NBR NUMBER,
  STATION_NBR NUMBER,
  
  CONSTRAINT fk_product FOREIGN KEY (STORE_NBR)
    REFERENCES PRODUCT(STORE_NBR),
    
  CONSTRAINT fk_weather FOREIGN KEY (STATION_NBR)
    REFERENCES departments(STATION_NBR)
);