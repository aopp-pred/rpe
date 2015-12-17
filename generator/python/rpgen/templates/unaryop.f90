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
    ELEMENTAL FUNCTION {{ operator.name }}_{{ type1.name }} (x) RESULT (z)
        {{ type1.declaration }}, INTENT(IN) :: x
        {{ operator.return_type.declaration }} :: z
        {% if operator.return_type.name == "rpe" %}
        z%sbits = significand_bits(x)
        {% endif %}
        z = {{ operator.operator }}(x{{ type1.accessor }})
    END FUNCTION {{ operator.name }}_{{ type1.name }}
