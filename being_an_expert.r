data = read.csv("�����2.csv", stringsAsFactors = FALSE)
data$adr = ""

for (i in 1:nrow(data)) {
  print(data$review[i])
  print("������� 1, ���� � ������ ����������� �������� �������,")
  print("������� 0 � ��������� ������,")
  print("������� -1, ���� �� �������,")
  print("������� 2, ���� �������� ������� ����������� � ��������")
  print("������� 3, ���� ������������� �������� ������� �� ��������� � ���������������� ���������")
  print("������� s, ���� ������ � ������ ����������:")
  n <- readline(prompt="����:  ")
  if (n == "s") {
    stop = i
    break
  }
  data$adr[i] = n
}


### stop - ����� ������, ��� ������������