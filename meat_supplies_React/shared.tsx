export const COLOR_BY_TYPE = {
  plant: "#c1c4b7",
  animal: "#d9a9a1"
};

export function colorCoding(type) {
  return COLOR_BY_TYPE[type] || "#eddbac";
}

export const COLOR_BY_TYPE_SUMMARY = {
  plant: "#889181",
  animal: "#b85b54"
};
export function colorCodingSummary(type) {
  return COLOR_BY_TYPE_SUMMARY[type] || "#E1BE6A";
}
export const NAME_BY_TYPE = {
  plant: ["plant-", "based"],
  animal: ["animal-", "based"]
};

export function typeCoding(type) {
  return NAME_BY_TYPE[type] || ["oils", "& fats"];
}

export interface TooltipData {
  x: number | string;
  y: number | string;
  // displayValue: string;
  datapoint: PlanetaryPoint;
  // energyFilter: boolean;
}

export interface TooltipDataCountry {
  x: number | string;
  y: number | string;
  // displayValue: string;
  datapoint: PlanetaryPoint;
  lancet: PlanetaryPoint;
  isMobile: boolean;
  // energyFilter: boolean;
}

interface PlanetaryPoint {
  food: string;
  grams: number;
  calories: number;
  type: string;
}
