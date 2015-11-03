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
    {% macro dimension(n) -%}
    {% if ndim != 0 -%}
    , DIMENSION({{ ([':'] * n)|join(', ') }})
    {%- endif %}
    {%- endmacro %}
    {% macro dimension_full(n) -%}
    {% if ndim != 0 -%}
    , DIMENSION({% for i in range(n) -%}SIZE(a, {{ i + 1 }}){% if loop.last %}{% else %}, {% endif %}{%- endfor %})
    {%- endif %}
    {%- endmacro %}
    FUNCTION {{ function.name }}_{{ type1.name }}_{{ ndim }}d (a) RESULT (x)
        {{ type1.declaration }}{{ dimension(ndim) }}, INTENT(IN) :: a
        {{ function.return_type.declaration }} :: x
        REAL(KIND=RPE_REAL_KIND){{ dimension_full(ndim) }} :: t
        t = a
        x = {{ function.name.upper() }}(t)
        {% if function.reduce_precision %}
        CALL reduce_precision (x)
        {% endif %}
    END FUNCTION {{ function.name }}_{{ type1.name }}_{{ ndim }}d
