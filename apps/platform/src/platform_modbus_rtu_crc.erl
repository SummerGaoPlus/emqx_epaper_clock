%%--------------------------------------------------------------------
% Copyright (c) 2024-2025 SummerGao <summergao@vip.qq.com>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%%--------------------------------------------------------------------

-module(platform_modbus_rtu_crc).
-export([check_crc/1, calculate_crc/1, test/0]).

%% CRC 校验
-spec check_crc(list()) -> boolean().
check_crc(Data) ->
  {H, [CH,CL]} = lists:split(length(Data) - 2, Data),
  CRC = calculate_crc(H),
  CRC1 = (CH bsl 8) + CL,
  CRC =:= CRC1.

% 计算ModeBus RTU CRC校验码
-spec calculate_crc(list()) -> integer().
calculate_crc(Data) ->
  calculate_crc(Data, 16#FFFF).

calculate_crc([], CRC) ->
  % 交换CRC的高位和低位字节
  CRC1 = ((CRC band 16#FF) bsl 8) bor ((CRC bsr 8) band 16#FF),
  CRC1;
calculate_crc([Byte | Rest], CRC) ->
  CRC1 = CRC bxor (Byte band 16#FF),
  CRC2 = calculate_crc_(8, CRC1),
  calculate_crc(Rest, CRC2).

calculate_crc_(0, CRC) ->
  CRC;
calculate_crc_(N, CRC) ->
  NextCRC = if (CRC band 16#0001) /= 0 -> (CRC bsr 1) bxor 16#A001; true -> CRC bsr 1 end,
  calculate_crc_(N - 1, NextCRC).

test() ->
  Data = [16#01, 16#03, 16#00, 16#00, 16#00, 16#02],
  CRC = calculate_crc(Data),
  io:format("CRC CheckCode: ~p~n", [CRC]).
