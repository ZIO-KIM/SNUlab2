

## 데이터 재 마운트 후, 기본 windowing, mass 데이터 생성하기 ##
1. dicom2HU_v2_save_trainData.ipynb / dicom2HU_v2_save_testData.ipynb 로 windowing & dicom to png 변경
오래 걸림 (2~3시간) 
-> kidneyData_windowing 폴더 생성됨

2. kidney_json_extract_mass-instanceNumber.ipynb 로 mass 부분만 추출 
-> kidneyData_windowing_mass 폴더 생성됨

3. create kidneyData_windowing_mass_total (train+test).ipynb로 train+test 합치기, ART/PRE로만 나눠짐
-> kidneyData_windowing_mass_total 폴더 생성됨

4. Draw mass_only - ZIO.ipynb -> mass 부분만 떼서 저장하는 코드 (이미지 자체에 mass 밖에 없도록 저장함)


## CycleGAN tfds build ##
1. terminal -> activate kernel ddd
2. cd blocks1 -> tfds new kidney_mass -> 생성된 kidney_mass 폴더에서 kidney_mass.py 클릭
3. _info class 안의 label을 'ART', 'PRE'로 변경
   _split generators 안의 경로 및 이름 변경
4. cd kidney_mass
5. tfds build (오래 걸림, 1시간 이상) 


## CycleGAN ##