# MIPS_VideoCharMap

**Trabalho final da disciplina Laboratório de Arquitetura e Organização de Computadores 1**

Autores:
  * Gabriel Alves
  * Anderson Montor
  * Alisson Gomes 

O objetivo deste trabalho é implementar o Charmap em VGA para a arquitetura da MIPS multiciclo na linguagem SystemVerilog, e após terminado o processador será capaz de imprimir caracteres ASCII em vídeo a partir da leitura de uma memória de dados externa, no caso de uma FPGA Altera DE1.

Para tal, precisaremos implementar as seguintes funções na arquitetura MIPS:
  * Controle de exceções
  * Syscalls (read_string e print_string)
  * Memória de dados externa
  * Mapear os caracteres ASCII para VGA

Ao início do projeto, recebemos o código do processador Multiciclo básico funcional com todos os módulos, e também o código para o monitor VGA básico.
  
*São Carlos, 2017*
