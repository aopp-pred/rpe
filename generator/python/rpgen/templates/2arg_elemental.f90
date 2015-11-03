{#
  Copyright 2015 Andrew Dawson, Peter Dueben

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
#}
    ELEMENTAL FUNCTION {{ function.name }}_{{ type1.name }}_{{ type2.name }} (a, b) RESULT (x)
        {{ type1.declaration }}, INTENT(IN) :: a
        {{ type2.declaration }}, INTENT(IN) :: b
        {{ function.return_type.declaration }} :: x
        x = {{ function.name.upper() }}(a{{ type1.accessor }}, b{{ type2.accessor }})
    END FUNCTION {{ function.name }}_{{ type1.name }}_{{ type2.name }}
