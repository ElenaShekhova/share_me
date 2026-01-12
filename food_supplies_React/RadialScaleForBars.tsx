import React, { type FC } from "react";

interface RadialScaleForBarsProps {
  yScale: any;
  energyFilter: string;
  isMobile: boolean;
  withText: boolean;
}

export const RadialScaleForBars: FC<RadialScaleForBarsProps> = ({
  yScale,
  energyFilter,
  isMobile,
  withText = false
}) => {
  const gram = "gram";

  const arrayGram = energyFilter === gram ? [100, 200, 300] : [600, 400, 200];
  return (
    <>
      {arrayGram.map((v, i) => {
        const r = yScale(v);
        const angleDeg = -90;
        const labelOffset = 6;

        return (
          <g key={i}>
            <circle
              cx={0}
              cy={0}
              r={r}
              fill="none"
              stroke="#575454"
              strokeWidth={0.5}
              strokeOpacity={0.5}
              pointerEvents="none"
              strokeDasharray="6 2"
            />
            {withText && (
              <g transform={`rotate(${angleDeg}) translate(${r + labelOffset}, 0)`}>
                <text
                  className={"font-body"}
                  fill={"rgba(0,0,0,0.8)"}
                  fontSize={isMobile ? 8 : 10}
                  alignmentBaseline="middle"
                  textAnchor="middle"
                  transform="rotate(90)" // counter-rotate to keep text horizontal
                >
                  {`${v}${energyFilter === gram ? "g" : "kcal"}`}
                </text>
              </g>
            )}
          </g>
        );
      })}
    </>
  );
};
