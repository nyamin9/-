# 🎰 01. A/B 테스트  

🎲 실험설계는 대부분의 응용 연구 분야에서 통계분석의 토대가 됨  

🎲 어떤 가설을 확인하거나 기각하기 위한 목표를 가지고 있음  

🎲 모든 통계적 추론 과정은 가설을 세우는 것에서 시작함  

🎲 추론 - 제한된 데이터로 주어진 실험 결과를 더 큰 과정 혹은 모집단에 적용하려는 의도를 반영함  

📌 가설 수립 -> 실험 설계 -> 데이터 수집 -> 추론 및 결론 도출  

<br>  


🎲 A/B 테스트 : 두 가지 처리 방법,제품,절차 중 어느 쪽이 다른 쪽보다 우월하다는 것을 입증하고자 실험군을 두 그룹으로 나눠 진행하는 실험  

🎲 연구 대상을 두 가지 이상의 그룹 중 하나에 할당하고, 서로 다른 처리 조건을 제외한 나머지 조건은 모두 동일하게 처리함  

🎲 그 결과를 쉽게 측정할 수 있으므로 웹 디자인이나 마케팅에서 일반적으로 사용됨  


<br>  


🎲 용어 정리  

- 처리 : 어떤 대상에 주어지는 특별한 환경이나 조건  
   
- 처리군 : 특정 처리에 노출된 대상들의 집단  
   
- 대조군 : 어떤 처리도 하지 않은 대상들의 집단  
   
- 랜덤화 : 처리를 적용할 대상을 임의로 결정하는 과정  
   
- 대상 : 처리를 적용할 개체 대상 - 피실험자  
   
- 검정통계량 : 처리 효과를 측정하기 위한 지표  
   
<br>  

🎲 A/B 테스트의 몇 가지 예  

- 종자 발아가 더 잘되는 환경을 알아보기 위해 두 가지 토양 처리를 검정함  
   
- 암을 더 효과적으로 억제하는 두 가지 치료법 검정  
   
- 두 가지 가격을 검정하여 더 많은 순이익을 산출하는 쪽을 검정  
   
- 두 인터넷 뉴스 제목을 검정하여 더 많은 클릭을 생성하는 쪽을 결정  
   
- 두 개의 인터넷 광고를 검정하여 어느 것이 더 높은 전환율을 얻을지 판단  
   
<br>  

🎲 A/B 테스트의 주요 개념  

- 둘 중 하나의 처리를 할당할 대상이 주어짐 -> 피험자가 어느 특정 처리에 노출됨을 의미함  
    
- 이상적으로, 피험자는 랜덤하게 어느 처리에 할당됨  
   
- 위의 두 조건에 의해 처리군 간의 차이는 아래 두 가지 이유 중 하나로 인식됨  
   
  - 다른 임의의 효과  
     
  - 각 대상이 각각의 처리에 배정될지에 대한 경우의 수  
       
- 그룹 A와 그룹 B를 비교하는 데 사용되는 검정통계량 혹은 측정 지표에 주의를 기울여야 함  
   
  - 일반적으로 사용하는 지표는 클릭/클릭하지 않음, 구매/구매하지 않음 등의 이진 변수로 나타남  
     
  - 이러한 결과를 2x2 표로 요약할 수 있음   
      
  - 결국 A/B 테스트의 실험 결과를 통해 처리 A와 B 사이의 결정을 내릴 것이기 때문에 단일 지표 혹은 검정통계량을 사전에 미리 정해 두어야 함  
     
  - 먼약 실험 수행 후 검정통계량을 선택한다면 연구자 편향이라는 함정에 빠지게 됨  
   
     
<br>  

## 🎰 01.01. 대조군이 필요한 이유  

🎲 대조군 없이 처리를 적용한 그룹의 결과를 단순히 이전 결과와 비교하면 안 되는 이유  

- 대조군이 없다면 '처리를 제외한 다른 모든 조건은 동일하다' 는 조건을 보장할 수 없으며 두 결과의 차이가 처리에 의한 것인지 우연에 의한 것인지 확신할 수 없음.  
   
- 대조군의 경우 처리를 제외한 나머지는 처리군과 동일한 조건이 적용됨  
   
- 결론적으로 단순히 이전 결과와 비교할 경우 처리 이외의 다은 요소가 다를 수 있기 때문에 대조군의 설정이 필요함  
   
<br>  

***  
