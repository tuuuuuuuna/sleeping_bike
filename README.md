---
잠자는 따릉이를 깨워라

[문제 제기](#문제-제기)

[TO-BE](#to-be)

[나의 역할](#나의-역할)

[성과분석](#성과-분석)

[한계](#한계)

---







# 잠자는 따릉이를 깨워라

# 문제 제기

시간대를 고려하지 않은 재배치

- 자전거가 없어 빌리지 못하는 **재고부족문제** 발생
- 자전거가 과다하게 비치되는 **잉여자원** 발생

---

# TO-BE

예측 모형을 이용한 **새로운 배치 기준** 제시

- 랜덤포레스트 모형을 이용하여 설계
- 시간대 뿐 아니라 날씨, 휴일 여부 등의 다양한 요소 고려



더욱 효율적인 **재배치 경로** 제시

- 재배치 기준과 **실시간 거치정도**를 고려해 최적화 된 경로를 제시

- 재배치가 필요한 정류소와 그렇지 않은 정류소 구분

- **유전 알고리즘**을 이용하여 최적의 재배치 경로 산출

---

# 나의 역할

- 웹 크롤러 코딩 (파이썬)
- 데이터분석 모델 설계 및 구현 (R)
- 결과물 시각화 (R)

---

# 성과 분석

#### 대여 및 수요 예측 모형 구현

- 정류소별 **평균 70%** 예측 정확성
- 정류소 별 예측치 예시

![정류소 별 예측 정확도](https://user-images.githubusercontent.com/68371545/98894414-1350b100-24e8-11eb-8540-4ecf7b38edfe.png)



#### 실시간 상황을 고려한 최적의 **재배치 경로** 제시

- 개선 전 경로의 시각화

![개선전 재배치 경로](https://user-images.githubusercontent.com/68371545/98894788-f10b6300-24e8-11eb-866a-bee20dae6069.jpg)

- 제시한 기간 동안(2019.10.01 ~ 10.7) 아침, 점심, 저녁 총 21타임의 경로 제시
- **최대 39.38%** 의 경로 단축
- 가장 많이 단축된 경로의 시각화

![개선후 재배치 경로](https://user-images.githubusercontent.com/68371545/98894791-f23c9000-24e8-11eb-9487-99735c1e40d7.jpg)

---

# 한계

- 마포구로 한정 된 범위에서만 사용 가능한 모델
- 유전 알고리즘에서 사용된 거리는 지도상의 직선거리를 이용
